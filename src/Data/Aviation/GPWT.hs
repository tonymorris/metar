{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

{- FOURMOLU_DISABLE -}
-- $setup
-- >>> import Text.HTML.TagSoup (parseTags)
{- FOURMOLU_ENABLE -}

-- | Fetching Grid Point Wind and Temperature forecasts (GPWTs) from the
-- Australian Bureau of Meteorology.
--
-- The BOM's aviation site publishes GPWT charts at
-- <https://www.bom.gov.au/aviation/charts/grid-point-forecasts/> in three
-- flight-level bands ('GPWTLow', 'GPWTMid', 'GPWTHigh'). Each band offers a
-- number of area codes (e.g. @AUS@, @NSW@, @QLD-N@, @VIC\/TAS@, @TIMS@) and
-- eight three-hourly time slices (@00Z@, @03Z@ ... @21Z@). This module
-- parses the HTML page for the (level, area, time) → product-id map and
-- fetches the requested PNG.
module Data.Aviation.GPWT (
  -- * Types
  GPWTLevel (..),
  GPWTError (..),
  GPWTImage (..),
  GPWTEntry (..),

  -- * Fetching
  getGPWT,

  -- * Parsing (exported for testing)
  parseGPWTEntries,
  parseLevel,
  levelPrefix,
  levelAndTimeFromTitle,
  pidFromHref,
  normaliseCode,
  normaliseTime,
  renderGPWTError,
) where

import Control.Exception (catch)
import Control.Lens ((&), (.~), (^.))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace, toUpper)
import Data.List (nub, sort)
import Network.HTTP.Client (HttpException)
import Network.Wreq (Options, defaults, getWith, headers, responseBody)
import Text.HTML.TagSoup (Tag (TagOpen, TagText), parseTags)

-- | The three flight-level bands published on the GPWT page.
--
-- >>> [GPWTLow, GPWTMid, GPWTHigh]
-- [GPWTLow,GPWTMid,GPWTHigh]
data GPWTLevel
  = GPWTLow
  | GPWTMid
  | GPWTHigh
  deriving (Eq, Show)

-- | One (level, area, time) product parsed from the GPWT page.
--
-- The @gpwtEntryPid@ is the bare product identifier (without the @.png@
-- suffix), e.g. @\"IDY04650\"@.
data GPWTEntry = GPWTEntry
  { gpwtEntryLevel :: GPWTLevel
  , gpwtEntryArea :: String
  , gpwtEntryTime :: String
  , gpwtEntryPid :: String
  }
  deriving (Eq, Show)

-- | One raw GPWT image response.
data GPWTImage = GPWTImage
  { gpwtContentType :: String
  , gpwtBytes :: BL.ByteString
  }
  deriving (Eq, Show)

-- | Something that can go wrong while retrieving a GPWT.
data GPWTError
  = -- | The URL segment for the level was not one of @low@, @mid@, @high@.
    GPWTUnknownLevel String
  | -- | No product for the requested (level, area, time) triple. The valid
    -- (area, [time]) pairs at that level are returned for the caller to
    -- render.
    GPWTNoSuchProduct GPWTLevel String String [(String, [String])]
  | -- | The BOM GPWT page did not contain any recognisable product entries.
    GPWTParseError
  | -- | A network request failed. First field is the source label.
    GPWTHttpError String String
  deriving (Eq, Show)

-- | Render a 'GPWTError' as a single line of human-readable text.
--
-- >>> renderGPWTError (GPWTUnknownLevel "xyz")
-- "unknown level \"xyz\" (valid: low, mid, high)"
--
-- >>> renderGPWTError (GPWTNoSuchProduct GPWTLow "ZZZ" "99Z" [("AUS", ["00Z","03Z"])])
-- "no low-level GPWT for area \"ZZZ\" time \"99Z\" (valid: AUS [00Z, 03Z])"
--
-- >>> renderGPWTError GPWTParseError
-- "no GPWT products found on grid-point-forecasts page"
--
-- >>> renderGPWTError (GPWTHttpError "grid-point-forecasts" "connection timeout")
-- "grid-point-forecasts: connection timeout"
renderGPWTError ::
  GPWTError ->
  String
renderGPWTError e =
  case e of
    GPWTUnknownLevel l ->
      "unknown level " <> show l <> " (valid: low, mid, high)"
    GPWTNoSuchProduct lv area time valid ->
      "no "
        <> levelPrefix lv
        <> "-level GPWT for area "
        <> show area
        <> " time "
        <> show time
        <> " (valid: "
        <> renderValid valid
        <> ")"
    GPWTParseError ->
      "no GPWT products found on grid-point-forecasts page"
    GPWTHttpError src msg ->
      src <> ": " <> msg
 where
  renderValid xs =
    intercalateSep "; " [a <> " [" <> intercalateSep ", " ts <> "]" | (a, ts) <- xs]
  intercalateSep _ [] = ""
  intercalateSep _ [x] = x
  intercalateSep sep (x : xs) = x <> sep <> intercalateSep sep xs

-- | Prefix used in BOM's title attributes (@\"Low-level\"@, @\"Mid-level\"@,
-- @\"High-level\"@) for the given 'GPWTLevel', lowercased.
--
-- >>> map levelPrefix [GPWTLow, GPWTMid, GPWTHigh]
-- ["low","mid","high"]
levelPrefix ::
  GPWTLevel ->
  String
levelPrefix GPWTLow = "low"
levelPrefix GPWTMid = "mid"
levelPrefix GPWTHigh = "high"

-- | Parse a URL segment (@low@\/@mid@\/@high@, case-insensitive) into a
-- 'GPWTLevel'.
--
-- >>> map parseLevel ["low", "Mid", "HIGH"]
-- [Just GPWTLow,Just GPWTMid,Just GPWTHigh]
--
-- >>> parseLevel "other"
-- Nothing
--
-- >>> parseLevel ""
-- Nothing
parseLevel ::
  String ->
  Maybe GPWTLevel
parseLevel s =
  case fmap toUpper s of
    "LOW" -> Just GPWTLow
    "MID" -> Just GPWTMid
    "HIGH" -> Just GPWTHigh
    _ -> Nothing

-- | Normalise an area code for comparison. Uppercases and drops every
-- character that is not a letter or digit, so callers can use any of
-- @\"VIC\/TAS\"@, @\"VIC-TAS\"@ or @\"VICTAS\"@ interchangeably.
--
-- >>> map normaliseCode ["VIC/TAS", "vic-tas", "victas", "QLD-N", "qldn"]
-- ["VICTAS","VICTAS","VICTAS","QLDN","QLDN"]
normaliseCode ::
  String ->
  String
normaliseCode =
  fmap toUpper . filter isAlnum
 where
  isAlnum c = isAsciiUpper c || isAsciiLower c || isDigit c

-- | Normalise a time slot for comparison. Uppercases, strips whitespace, and
-- pads a single leading zero if only one digit was given before the @Z@.
--
-- >>> map normaliseTime ["00Z", "03z", "9Z", "18Z", " 21z "]
-- ["00Z","03Z","09Z","18Z","21Z"]
--
-- >>> normaliseTime "notatime"
-- "NOTATIME"
normaliseTime ::
  String ->
  String
normaliseTime =
  let padZ s =
        case s of
          [d, 'Z'] | isDigit d -> ['0', d, 'Z']
          _ -> s
   in padZ . fmap toUpper . filter (not . isSpace)

-- | Extract the trailing @\"XXZ\"@ time from a BOM title attribute and pair
-- it with a 'GPWTLevel'. Titles look like @\"Low-level, Australia 00Z\"@.
--
-- >>> levelAndTimeFromTitle "Low-level, Australia 00Z"
-- Just (GPWTLow,"00Z")
--
-- >>> levelAndTimeFromTitle "Mid-level, North-East 12Z"
-- Just (GPWTMid,"12Z")
--
-- >>> levelAndTimeFromTitle "High-level, Tasman 21Z"
-- Just (GPWTHigh,"21Z")
--
-- >>> levelAndTimeFromTitle "Something else"
-- Nothing
--
-- Malformed times (BOM has one @\"015\"@ typo entry) are rejected:
--
-- >>> levelAndTimeFromTitle "Mid-level, South-East 015"
-- Nothing
levelAndTimeFromTitle ::
  String ->
  Maybe (GPWTLevel, String)
levelAndTimeFromTitle t =
  let (prefix, rest0) = break (== ',') t
      rest = dropWhile isSpace (drop 1 rest0)
   in do
        lv <- case prefix of
          "Low-level" -> Just GPWTLow
          "Mid-level" -> Just GPWTMid
          "High-level" -> Just GPWTHigh
          _ -> Nothing
        tm <- case reverse (words rest) of
          (w : _) | isValidTime w -> Just w
          _ -> Nothing
        Just (lv, tm)
 where
  isValidTime [d1, d2, 'Z'] = isDigit d1 && isDigit d2
  isValidTime _ = False

-- | Extract a product id (without the @.png@ suffix) from an anchor's
-- @href@ attribute.
--
-- >>> pidFromHref "/difacs/aviation/IDY04650.png"
-- Just "IDY04650"
--
-- >>> pidFromHref "/fwo/aviation/IDY04651.png"
-- Just "IDY04651"
--
-- >>> pidFromHref "/aviation/gaf/gaf.shtml"
-- Nothing
--
-- >>> pidFromHref ""
-- Nothing
pidFromHref ::
  String ->
  Maybe String
pidFromHref href =
  let base = reverse (takeWhile (/= '/') (reverse href))
   in case reverse base of
        'g' : 'n' : 'p' : '.' : rest ->
          let name = reverse rest
           in if not (null name) && all (\c -> isAsciiUpper c || isDigit c) name
                then Just name
                else Nothing
        _ -> Nothing

-- | Walk a parsed HTML tag stream and emit one 'GPWTEntry' per
-- @\<a class=\"loc\"\>@ product link. Area codes are inherited from the
-- most recently opened @\<li\>@; the level and time come from each
-- anchor's own @title@ attribute.
--
-- >>> parseGPWTEntries (parseTags "<ul><li>AUS: <a class=\"loc\" href=\"/difacs/aviation/IDY04650.png\" title=\"Low-level, Australia 00Z\">00Z</a></li></ul>")
-- [GPWTEntry {gpwtEntryLevel = GPWTLow, gpwtEntryArea = "AUS", gpwtEntryTime = "00Z", gpwtEntryPid = "IDY04650"}]
--
-- >>> length (parseGPWTEntries (parseTags "<ul><li>AUS: <a class=\"loc\" href=\"/difacs/aviation/IDY04650.png\" title=\"Low-level, Australia 00Z\">00Z</a> <a class=\"loc\" href=\"/difacs/aviation/IDY04651.png\" title=\"Low-level, Australia 03Z\">03Z</a></li></ul>"))
-- 2
--
-- >>> parseGPWTEntries []
-- []
--
-- Anchors without a preceding @\<li\>@ header are skipped:
--
-- >>> parseGPWTEntries (parseTags "<a class=\"loc\" href=\"/difacs/aviation/IDY04650.png\" title=\"Low-level, Australia 00Z\">00Z</a>")
-- []
parseGPWTEntries ::
  [Tag String] ->
  [GPWTEntry]
parseGPWTEntries =
  let go _ [] = []
      go _ (TagOpen "li" _ : ts) =
        let (code, ts') = takeAreaCode ts
         in go (Just code) ts'
      go area (TagOpen "a" attrs : ts)
        | lookup "class" attrs == Just "loc"
        , Just href <- lookup "href" attrs
        , Just title <- lookup "title" attrs
        , Just pid <- pidFromHref href
        , Just (lv, tm) <- levelAndTimeFromTitle title
        , Just areaC <- area =
            GPWTEntry lv areaC tm pid : go area ts
      go area (_ : ts) = go area ts
   in go Nothing

-- | Consume the first non-whitespace text node after a @\<li\>@ open tag
-- and return the trimmed content up to (but not including) any @\':\'@.
--
-- >>> takeAreaCode (parseTags "AUS: some link")
-- ("AUS",[TagText " some link"])
--
-- >>> takeAreaCode (parseTags " NE:")
-- ("NE",[TagText ""])
--
-- >>> fst (takeAreaCode (parseTags "VIC/TAS:"))
-- "VIC/TAS"
--
-- >>> takeAreaCode []
-- ("",[])
takeAreaCode ::
  [Tag String] ->
  (String, [Tag String])
takeAreaCode ts0 =
  case dropWhile isBlankText ts0 of
    TagText t : rest ->
      let (before, after) = break (== ':') t
          code = strip before
          rest' = case after of
            ':' : more -> TagText more : rest
            _ -> rest
       in (code, rest')
    _ -> ("", ts0)
 where
  isBlankText (TagText s) = all isSpace s
  isBlankText _ = False
  strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | HTTP options used when talking to bom.gov.au. Sending a browser-shaped
-- User-Agent (and the @check=ok@ cookie the site expects) is necessary to
-- avoid the anti-scraping block page.
bomOptions ::
  Options
bomOptions =
  defaults
    & headers
      .~ [ ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:120.0) Gecko/20100101 Firefox/120.0")
         , ("Accept", "*/*")
         , ("Accept-Language", "en-US,en;q=0.5")
         , ("Cookie", "check=ok")
         ]

-- | Fetch a GPWT image for a given level, area code, and time slot.
--
-- Area codes are matched via 'normaliseCode' (case-insensitive, punctuation
-- ignored) so @\"VIC/TAS\"@, @\"vic-tas\"@ and @\"VICTAS\"@ all resolve to
-- the same product. Time slots are normalised with 'normaliseTime' so
-- @\"9Z\"@, @\"09z\"@ and @\"09Z\"@ are equivalent.
--
-- >>> :t getGPWT
-- getGPWT
--   :: GPWTLevel -> String -> String -> IO (Either GPWTError GPWTImage)
getGPWT ::
  GPWTLevel ->
  String ->
  String ->
  IO (Either GPWTError GPWTImage)
getGPWT level areaIn timeIn =
  fetchEntries >>= \case
    Left err -> pure (Left err)
    Right es ->
      let wantArea = normaliseCode areaIn
          wantTime = normaliseTime timeIn
          matches =
            [ e
            | e <- es
            , gpwtEntryLevel e == level
            , normaliseCode (gpwtEntryArea e) == wantArea
            , normaliseTime (gpwtEntryTime e) == wantTime
            ]
       in case matches of
            (e : _) -> fetchImage (gpwtEntryPid e)
            [] -> pure (Left (GPWTNoSuchProduct level areaIn timeIn (validPairs level es)))

-- | Collect @(area, [times])@ pairs for a given level, in the order they
-- first appeared, with times sorted lexicographically.
--
-- >>> validPairs GPWTLow [GPWTEntry GPWTLow "AUS" "03Z" "A", GPWTEntry GPWTLow "AUS" "00Z" "B", GPWTEntry GPWTHigh "AUS" "00Z" "C"]
-- [("AUS",["00Z","03Z"])]
validPairs ::
  GPWTLevel ->
  [GPWTEntry] ->
  [(String, [String])]
validPairs level es =
  let filtered = filter (\e -> gpwtEntryLevel e == level) es
      areas = nub (fmap gpwtEntryArea filtered)
   in [ (a, sort (nub [gpwtEntryTime e | e <- filtered, gpwtEntryArea e == a]))
      | a <- areas
      ]

-- | Fetch and parse the GPWT landing page for its full product list.
fetchEntries ::
  IO (Either GPWTError [GPWTEntry])
fetchEntries =
  let url = "https://www.bom.gov.au/aviation/charts/grid-point-forecasts/"
   in httpGet "grid-point-forecasts" url >>= \case
        Left err -> pure (Left err)
        Right body ->
          case parseGPWTEntries (parseTags (BLC.unpack body)) of
            [] -> pure (Left GPWTParseError)
            xs -> pure (Right xs)

-- | Fetch the actual GPWT PNG for a given product id. BOM serves @IDY*@
-- products from @\/fwo\/aviation\/@ and @IDX*@ products from
-- @\/difacs\/aviation\/@; the HTML's @href@ is not reliable across both
-- families, so we route by product-id prefix instead of trusting it.
fetchImage ::
  String ->
  IO (Either GPWTError GPWTImage)
fetchImage pid =
  let url = "https://www.bom.gov.au" <> imagePath pid <> pid <> ".png"
   in httpGet pid url >>= \case
        Left err -> pure (Left err)
        Right body -> pure (Right (GPWTImage "image/png" body))

-- | Choose the URL base for a GPWT product id.
--
-- >>> imagePath "IDY04650"
-- "/fwo/aviation/"
--
-- >>> imagePath "IDX0129"
-- "/difacs/aviation/"
--
-- >>> imagePath "IDX0476"
-- "/difacs/aviation/"
--
-- >>> imagePath "OTHER"
-- "/difacs/aviation/"
imagePath ::
  String ->
  String
imagePath pid =
  case pid of
    'I' : 'D' : 'Y' : _ -> "/fwo/aviation/"
    _ -> "/difacs/aviation/"

-- | Wrap a Wreq GET call, catching 'HttpException's and returning them as a
-- 'GPWTHttpError' tagged with the given source label.
httpGet ::
  String ->
  String ->
  IO (Either GPWTError BL.ByteString)
httpGet src url =
  catch
    (fmap (Right . (^. responseBody)) (getWith bomOptions url))
    (\e -> pure (Left (GPWTHttpError src (show (e :: HttpException)))))
