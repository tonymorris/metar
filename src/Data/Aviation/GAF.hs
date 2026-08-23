{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

{- FOURMOLU_DISABLE -}
-- $setup
-- >>> import Text.HTML.TagSoup (parseTags)
{- FOURMOLU_ENABLE -}

-- | Fetching Graphical Area Forecasts (GAFs) from the Australian Bureau of
-- Meteorology.
--
-- The BOM's aviation site publishes a national GAF page at
-- <https://www.bom.gov.au/aviation/gaf/gaf.shtml> that lists ten forecast
-- areas (@WA-N@, @WA-S@, @NT@, @QLD-N@, @QLD-S@, @SA@, @NSW-W@, @NSW-E@,
-- @VIC@, @TAS@). For each area there are four rotating PNG products; which
-- product is \"current\" and which is \"next\" depends on the current UTC
-- hour. This module parses the HTML page for the valid area codes and the
-- accompanying JavaScript for the area→product mapping, then fetches the
-- appropriate PNG.
module Data.Aviation.GAF (
  -- * Types
  GAFPeriod (..),
  GAFError (..),
  GAFImage (..),

  -- * Fetching
  getGAF,

  -- * Parsing (exported for testing)
  parseAreaCodes,
  parsePids,
  normaliseArea,
  hourIndex,
  pickPid,
  renderGAFError,
) where

import Control.Exception (catch)
import Control.Lens ((&), (.~), (^.))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace, toUpper)
import Data.List (find)
import Data.Time.Clock (UTCTime (utctDayTime), getCurrentTime)
import Network.HTTP.Client (HttpException)
import Network.Wreq (Options, defaults, getWith, headers, responseBody)
import Text.HTML.TagSoup (Tag (TagOpen), parseTags)

-- | Which of the two forecast periods a caller wants.
--
-- >>> [GAFCurrent, GAFNext]
-- [GAFCurrent,GAFNext]
data GAFPeriod
  = GAFCurrent
  | GAFNext
  deriving (Eq, Show)

-- | One raw GAF image response.
data GAFImage = GAFImage
  { gafContentType :: String
  , gafBytes :: BL.ByteString
  }
  deriving (Eq, Show)

-- | Something that can go wrong while retrieving a GAF.
data GAFError
  = -- | Requested area code was not one of the codes exposed by the BOM
    -- GAF page. The list of valid codes is returned for the caller to render.
    GAFUnknownArea String [String]
  | -- | The BOM GAF HTML did not contain any recognisable area entries.
    GAFAreasParseError
  | -- | The BOM GAF JavaScript did not contain a product-id block for the
    -- (normalised) area name.
    GAFPidsParseError String
  | -- | A network request failed. First field is the source label.
    GAFHttpError String String
  deriving (Eq, Show)

-- | Render a 'GAFError' as a single line of human-readable text.
--
-- >>> renderGAFError (GAFUnknownArea "ZZZ" ["WA-N", "VIC"])
-- "unknown area \"ZZZ\" (valid: WA-N, VIC)"
--
-- >>> renderGAFError GAFAreasParseError
-- "no GAF areas found on gaf.shtml"
--
-- >>> renderGAFError (GAFPidsParseError "WAN")
-- "no product ids for area WAN in gaf-pub.js"
--
-- >>> renderGAFError (GAFHttpError "gaf.shtml" "connection timeout")
-- "gaf.shtml: connection timeout"
renderGAFError ::
  GAFError ->
  String
renderGAFError e =
  case e of
    GAFUnknownArea a valid ->
      "unknown area " <> show a <> " (valid: " <> commaList valid <> ")"
    GAFAreasParseError ->
      "no GAF areas found on gaf.shtml"
    GAFPidsParseError a ->
      "no product ids for area " <> a <> " in gaf-pub.js"
    GAFHttpError src msg ->
      src <> ": " <> msg
 where
  commaList [] = ""
  commaList [x] = x
  commaList (x : xs) = x <> ", " <> commaList xs

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

-- | Extract the GAF area codes from the @gaf.shtml@ page. The relevant
-- markup looks like
--
-- @
-- \<input type="checkbox" class="GAF" name="area[]" id="WA-N" value="IDN40000.txt" /\>
-- @
--
-- We keep every @class="GAF"@ checkbox except the invisible @areamap@ one.
--
-- >>> parseAreaCodes (parseTags "<input type=\"checkbox\" class=\"GAF\" id=\"WA-N\" /><input type=\"checkbox\" class=\"GAF\" id=\"VIC\" /><input type=\"checkbox\" class=\"GAF\" id=\"areamap\" />")
-- ["WA-N","VIC"]
--
-- >>> parseAreaCodes (parseTags "<input type=\"text\" id=\"WA-N\" />")
-- []
--
-- >>> parseAreaCodes []
-- []
parseAreaCodes ::
  [Tag String] ->
  [String]
parseAreaCodes =
  let isCheckbox attrs =
        lookup "type" attrs == Just "checkbox"
          && lookup "class" attrs == Just "GAF"
      go (TagOpen "input" attrs : rest)
        | isCheckbox attrs =
            case lookup "id" attrs of
              Just i | i /= "areamap" -> i : go rest
              _ -> go rest
      go (_ : rest) = go rest
      go [] = []
   in go

-- | Extract area→product-id mappings from the @gaf-pub.js@ script. Each area
-- has a JavaScript line of the form
--
-- @
-- WAN = ['IDY42054', 'IDY42055', 'IDY42056', 'IDY42057']; //area id #0 (WA - North)
-- @
--
-- We look for those four-element string-array assignments and key them by
-- the JS variable name. Duplicate keys keep the first occurrence.
--
-- >>> parsePids "WAN = ['IDY42054', 'IDY42055', 'IDY42056', 'IDY42057'];"
-- [("WAN",["IDY42054","IDY42055","IDY42056","IDY42057"])]
--
-- >>> parsePids "WAS=['A','B','C','D'];\nVIC = ['E','F','G','H'];"
-- [("WAS",["A","B","C","D"]),("VIC",["E","F","G","H"])]
--
-- Non-matching lines are ignored:
--
-- >>> parsePids "states = [WAN, WAS];"
-- []
--
-- >>> parsePids ""
-- []
parsePids ::
  String ->
  [(String, [String])]
parsePids src =
  [ (name, items)
  | line <- lines src
  , Just (name, items) <- [parsePidLine line]
  , length items == 4
  ]

-- | Parse a single @NAME = ['a','b','c','d'];@ style assignment. Returns
-- 'Nothing' if the line does not match that shape.
--
-- >>> parsePidLine "WAN = ['A', 'B', 'C', 'D'];"
-- Just ("WAN",["A","B","C","D"])
--
-- >>> parsePidLine "  QLDN=['A','B','C','D'] ;"
-- Just ("QLDN",["A","B","C","D"])
--
-- >>> parsePidLine "states = [WAN, WAS];"
-- Nothing
--
-- >>> parsePidLine ""
-- Nothing
parsePidLine ::
  String ->
  Maybe (String, [String])
parsePidLine s0 =
  let s = dropWhile isSpace s0
      (name, rest1) = span isIdent s
   in if null name
        then Nothing
        else
          let rest2 = dropWhile isSpace rest1
           in case rest2 of
                '=' : rest3 ->
                  let rest4 = dropWhile isSpace rest3
                   in case rest4 of
                        '[' : rest5 ->
                          case readStringArray rest5 of
                            Just items -> Just (name, items)
                            Nothing -> Nothing
                        _ -> Nothing
                _ -> Nothing
 where
  isIdent c = c == '_' || isAsciiUpper c || isAsciiLower c || isDigit c

-- | Read a comma-separated list of quoted strings, terminated by @]@.
-- Returns 'Nothing' if the syntax does not match.
--
-- >>> readStringArray "'a', 'b', 'c']"
-- Just ["a","b","c"]
--
-- >>> readStringArray "\"a\", \"b\"]"
-- Just ["a","b"]
--
-- >>> readStringArray "]"
-- Just []
--
-- >>> readStringArray "not-a-string]"
-- Nothing
readStringArray ::
  String ->
  Maybe [String]
readStringArray s =
  case dropWhile isSpace s of
    ']' : _ -> Just []
    '\'' : rest -> takeQuoted '\'' rest
    '"' : rest -> takeQuoted '"' rest
    _ -> Nothing
 where
  takeQuoted q rest =
    let (item, rest') = break (== q) rest
     in case rest' of
          _ : rest'' ->
            let rest''' = dropWhile isSpace rest''
             in case rest''' of
                  ',' : more ->
                    fmap (item :) (readStringArray more)
                  ']' : _ ->
                    Just [item]
                  _ -> Nothing
          [] -> Nothing

-- | Convert an area code from BOM UI form (@\"WA-N\"@) to the JavaScript
-- variable name form (@\"WAN\"@) used in @gaf-pub.js@.
--
-- >>> normaliseArea "WA-N"
-- "WAN"
--
-- >>> normaliseArea "QLD-S"
-- "QLDS"
--
-- >>> normaliseArea "VIC"
-- "VIC"
--
-- >>> normaliseArea "nsw-w"
-- "NSWW"
normaliseArea ::
  String ->
  String
normaliseArea =
  fmap toUpper . filter (\c -> c /= '-' && c /= ' ')

-- | Map a UTC hour (0-23) and a requested period to an index into the
-- four-element product list. The BOM rotation happens at 05, 11, 17 and 23
-- UTC.
--
-- >>> map (\h -> hourIndex h GAFCurrent) [0, 4, 5, 10, 11, 16, 17, 22, 23]
-- [3,3,0,0,1,1,2,2,3]
--
-- >>> map (\h -> hourIndex h GAFNext) [0, 4, 5, 10, 11, 16, 17, 22, 23]
-- [0,0,1,1,2,2,3,3,0]
hourIndex ::
  Int ->
  GAFPeriod ->
  Int
hourIndex h p =
  let base
        | h >= 5 && h < 11 = 0
        | h >= 11 && h < 17 = 1
        | h >= 17 && h < 23 = 2
        | otherwise = 3
   in case p of
        GAFCurrent -> base
        GAFNext -> (base + 1) `mod` 4

-- | Select the appropriate product id from a four-element list given a UTC
-- hour and the requested period.
--
-- >>> pickPid 6 GAFCurrent ["a", "b", "c", "d"]
-- Just "a"
--
-- >>> pickPid 6 GAFNext ["a", "b", "c", "d"]
-- Just "b"
--
-- >>> pickPid 23 GAFCurrent ["a", "b", "c", "d"]
-- Just "d"
--
-- >>> pickPid 23 GAFNext ["a", "b", "c", "d"]
-- Just "a"
--
-- >>> pickPid 6 GAFCurrent ["only-one"]
-- Nothing
pickPid ::
  Int ->
  GAFPeriod ->
  [String] ->
  Maybe String
pickPid h p pids =
  let go 0 (x : _) = Just x
      go n (_ : xs) = go (n - 1) xs
      go _ [] = Nothing
   in if length pids >= 4
        then go (hourIndex h p) pids
        else Nothing

-- | Fetch either the current or next GAF image for the given area code.
--
-- The area code must be one of the codes advertised on @gaf.shtml@
-- (@WA-N@, @WA-S@, @NT@, @QLD-N@, @QLD-S@, @SA@, @NSW-W@, @NSW-E@, @VIC@,
-- @TAS@); it is matched case-insensitively.
--
-- >>> :t getGAF
-- getGAF :: String -> GAFPeriod -> IO (Either GAFError GAFImage)
getGAF ::
  String ->
  GAFPeriod ->
  IO (Either GAFError GAFImage)
getGAF areaIn period =
  let want = fmap toUpper areaIn
   in fetchAreas >>= \case
        Left err -> pure (Left err)
        Right areas ->
          case find (\a -> fmap toUpper a == want) areas of
            Nothing -> pure (Left (GAFUnknownArea areaIn areas))
            Just areaCode ->
              fetchPids >>= \case
                Left err -> pure (Left err)
                Right pidMap ->
                  let key = normaliseArea areaCode
                   in case lookup key pidMap of
                        Nothing -> pure (Left (GAFPidsParseError key))
                        Just pids ->
                          do
                            h <- currentUtcHour
                            case pickPid h period pids of
                              Nothing -> pure (Left (GAFPidsParseError key))
                              Just pid -> fetchImage pid

-- | Fetch and parse the GAF landing page for its list of area codes.
fetchAreas ::
  IO (Either GAFError [String])
fetchAreas =
  let url = "https://www.bom.gov.au/aviation/gaf/gaf.shtml"
   in httpGet "gaf.shtml" url >>= \case
        Left err -> pure (Left err)
        Right body ->
          case parseAreaCodes (parseTags (BLC.unpack body)) of
            [] -> pure (Left GAFAreasParseError)
            xs -> pure (Right xs)

-- | Fetch and parse the GAF publication script for its area→PID map.
fetchPids ::
  IO (Either GAFError [(String, [String])])
fetchPids =
  let url = "https://www.bom.gov.au/scripts/aviation/forecasts/gaf-pub.js"
   in httpGet "gaf-pub.js" url >>= \case
        Left err -> pure (Left err)
        Right body -> pure (Right (parsePids (BLC.unpack body)))

-- | Fetch the actual GAF PNG for a given product id.
fetchImage ::
  String ->
  IO (Either GAFError GAFImage)
fetchImage pid =
  let url = "https://www.bom.gov.au/fwo/aviation/" <> pid <> ".png"
   in httpGet pid url >>= \case
        Left err -> pure (Left err)
        Right body -> pure (Right (GAFImage "image/png" body))

-- | Wrap a Wreq GET call, catching 'HttpException's and returning them as a
-- 'GAFHttpError' tagged with the given source label.
httpGet ::
  String ->
  String ->
  IO (Either GAFError BL.ByteString)
httpGet src url =
  catch
    (fmap (Right . (^. responseBody)) (getWith bomOptions url))
    (\e -> pure (Left (GAFHttpError src (show (e :: HttpException)))))

-- | Read the current UTC hour of day.
--
-- >>> :t currentUtcHour
-- currentUtcHour :: IO Int
currentUtcHour ::
  IO Int
currentUtcHour =
  do
    now <- getCurrentTime
    pure (floor (utctDayTime now / 3600) `mod` 24)
