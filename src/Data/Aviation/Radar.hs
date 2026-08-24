{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

{- FOURMOLU_DISABLE -}
-- $setup
-- >>> import Text.HTML.TagSoup (parseTags)
{- FOURMOLU_ENABLE -}

-- | Fetching radar and rainfall images from the Australian Bureau of
-- Meteorology.
--
-- The BOM's radar landing page at
-- <https://reg.bom.gov.au/australia/radar/> advertises the ~67 radar sites
-- as HTML @\<area\>@ elements whose @href@ points at the 128 km loop page
-- for that site (e.g. @\/products\/IDR023.loop.shtml@ for Melbourne). This
-- module parses that page for the (site, base id) list, then exposes two
-- fetch modes:
--
-- * 'RadarSingle' returns BOM's own single-frame @.gif@ served at
--   @\/radar\/\<PRODUCT\>.gif@ (verbatim, one image).
--
-- * 'RadarLoop' downloads all frames referenced by the site's
--   @.loop.shtml@ page, composites each on top of the site's background
--   and overlay transparencies (locations, range rings, catchments), and
--   assembles the frames into a single animated GIF.
--
-- A given radar site has nine per-site products (four range rings, one
-- doppler wind, four rainfall accumulations) plus the whole-of-Australia
-- @national@ product which is not tied to a site.
module Data.Aviation.Radar (
  -- * Types
  RadarPeriod (..),
  RadarKind (..),
  RadarProduct (..),
  RadarSite (..),
  RadarImage (..),
  RadarError (..),

  -- * Fetching
  getRadar,

  -- * Parsing (exported for testing)
  parseRadarSites,
  parseLoopFrames,
  parsePeriod,
  parseKind,
  parseProduct,
  findSite,
  productSuffix,
  productCode,
  transparenciesCode,
  slugify,
  renderRadarError,
) where

import Codec.Picture (
  DynamicImage,
  Image (imageHeight, imageWidth),
  Pixel8,
  PixelRGB8 (..),
  PixelRGBA8 (..),
  convertRGB8,
  convertRGBA8,
  decodePng,
  generateImage,
  pixelAt,
 )
import Codec.Picture.ColorQuant (defaultPaletteOptions, palettize)
import Codec.Picture.Gif (
  GifDelay,
  GifLooping (LoopingForever),
  encodeGifImages,
 )
import Codec.Picture.Types (Palette)
import Control.Exception (catch)
import Control.Lens ((&), (.~), (^.))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (isAlphaNum, isDigit, isSpace, toLower)
import Data.List (foldl', isPrefixOf)
import qualified Data.Set as S
import Network.HTTP.Client (HttpException)
import Network.Wreq (Options, defaults, getWith, headers, responseBody)
import Text.HTML.TagSoup (Tag (TagOpen), parseTags)

-- | Whether the caller wants a single-frame image or a loop.
--
-- >>> [RadarSingle, RadarLoop]
-- [RadarSingle,RadarLoop]
data RadarPeriod
  = RadarSingle
  | RadarLoop
  deriving (Eq, Show)

-- | Whether the caller wants a reflectivity/wind radar product or a
-- rainfall-accumulation product.
--
-- >>> [RadarKindRadar, RadarKindRainfall]
-- [RadarKindRadar,RadarKindRainfall]
data RadarKind
  = RadarKindRadar
  | RadarKindRainfall
  deriving (Eq, Show)

-- | One of the concrete BOM products the caller can ask for. The nine
-- per-site products share a base radar id (e.g. @\"IDR02\"@) and differ
-- only in their trailing suffix character; 'RadarNational' has no site.
--
-- >>> map productSuffix [Radar64km, Radar128km, Radar256km, Radar512kmComposite, RadarDopplerWind, Rain5m, Rain1h, RainSince9am, Rain24h]
-- ["4","3","2","1","I","A","B","C","D"]
data RadarProduct
  = Radar64km
  | Radar128km
  | Radar256km
  | Radar512kmComposite
  | RadarDopplerWind
  | RadarNational
  | Rain5m
  | Rain1h
  | RainSince9am
  | Rain24h
  deriving (Eq, Show)

-- | One radar site parsed from the BOM landing page. The base id is the
-- 128 km product code with its trailing @\'3\'@ removed, e.g. @\"IDR02\"@
-- for Melbourne (whose 128 km product is @IDR023@).
data RadarSite = RadarSite
  { radarSiteBase :: String
  , radarSiteName :: String
  , radarSiteSlug :: String
  }
  deriving (Eq, Show)

-- | One raw radar image response (either a single frame from BOM as a
-- @.gif@, or an animated GIF composited by this module).
data RadarImage = RadarImage
  { radarImageContentType :: String
  , radarImageBytes :: BL.ByteString
  }
  deriving (Eq, Show)

-- | Something that can go wrong while retrieving a radar image.
data RadarError
  = -- | Requested site was not one of the sites advertised on the BOM
    -- radar landing page. The valid list is returned for the caller to
    -- render.
    RadarUnknownSite String [RadarSite]
  | -- | Requested @\<image\>@ path segment was not valid for the given
    -- 'RadarKind'.
    RadarUnknownProduct RadarKind String
  | -- | Caller asked for a single-image @national@ product. Only the loop
    -- variant is published.
    RadarNationalNoSingle
  | -- | Landing page did not contain any recognisable @\<area\>@ site
    -- entries.
    RadarSitesParseError
  | -- | Loop page did not contain any @theImageNames[]@ frame URLs.
    RadarLoopFramesParseError String
  | -- | A PNG frame or transparency could not be decoded.
    RadarImageDecodeError String String
  | -- | Assembling the animated GIF failed.
    RadarGifEncodeError String
  | -- | A network request failed. First field is the source label.
    RadarHttpError String String
  deriving (Eq, Show)

-- | Render a 'RadarError' as a single line of human-readable text.
--
-- >>> renderRadarError (RadarUnknownProduct RadarKindRadar "17km")
-- "unknown radar product \"17km\" (valid: 64km, 128km, 256km, 512km-composite, doppler-wind, national)"
--
-- >>> renderRadarError (RadarUnknownProduct RadarKindRainfall "12h")
-- "unknown rainfall product \"12h\" (valid: 5m, 1h, since-9am, 24h)"
--
-- >>> renderRadarError RadarNationalNoSingle
-- "national radar has no single-frame image, only a loop"
--
-- >>> renderRadarError RadarSitesParseError
-- "no radar sites found on the BOM radar landing page"
--
-- >>> renderRadarError (RadarLoopFramesParseError "IDR023")
-- "no loop frames found on IDR023.loop.shtml"
--
-- >>> renderRadarError (RadarHttpError "IDR023.gif" "connection timeout")
-- "IDR023.gif: connection timeout"
renderRadarError ::
  RadarError ->
  String
renderRadarError e =
  case e of
    RadarUnknownSite s _ ->
      "unknown radar site " <> show s
    RadarUnknownProduct k p ->
      "unknown "
        <> kindWord k
        <> " product "
        <> show p
        <> " (valid: "
        <> commaList (validProducts k)
        <> ")"
    RadarNationalNoSingle ->
      "national radar has no single-frame image, only a loop"
    RadarSitesParseError ->
      "no radar sites found on the BOM radar landing page"
    RadarLoopFramesParseError pid ->
      "no loop frames found on " <> pid <> ".loop.shtml"
    RadarImageDecodeError src msg ->
      "failed to decode " <> src <> ": " <> msg
    RadarGifEncodeError msg ->
      "failed to encode animated GIF: " <> msg
    RadarHttpError src msg ->
      src <> ": " <> msg
 where
  kindWord RadarKindRadar = "radar"
  kindWord RadarKindRainfall = "rainfall"
  commaList [] = ""
  commaList [x] = x
  commaList (x : xs) = x <> ", " <> commaList xs

-- | List of accepted @\<image\>@ path segments for a given 'RadarKind'.
--
-- >>> validProducts RadarKindRadar
-- ["64km","128km","256km","512km-composite","doppler-wind","national"]
--
-- >>> validProducts RadarKindRainfall
-- ["5m","1h","since-9am","24h"]
validProducts ::
  RadarKind ->
  [String]
validProducts RadarKindRadar =
  ["64km", "128km", "256km", "512km-composite", "doppler-wind", "national"]
validProducts RadarKindRainfall =
  ["5m", "1h", "since-9am", "24h"]

-- | Parse a URL segment (@loop@ or @image@, case-insensitive) into a
-- 'RadarPeriod'.
--
-- >>> map parsePeriod ["loop", "Image", "LOOP"]
-- [Just RadarLoop,Just RadarSingle,Just RadarLoop]
--
-- >>> parsePeriod "other"
-- Nothing
parsePeriod ::
  String ->
  Maybe RadarPeriod
parsePeriod s =
  case fmap toLower s of
    "loop" -> Just RadarLoop
    "image" -> Just RadarSingle
    _ -> Nothing

-- | Parse a URL segment (@radar@ or @rainfall@, case-insensitive) into a
-- 'RadarKind'.
--
-- >>> map parseKind ["radar", "Rainfall", "RADAR"]
-- [Just RadarKindRadar,Just RadarKindRainfall,Just RadarKindRadar]
--
-- >>> parseKind "other"
-- Nothing
parseKind ::
  String ->
  Maybe RadarKind
parseKind s =
  case fmap toLower s of
    "radar" -> Just RadarKindRadar
    "rainfall" -> Just RadarKindRainfall
    _ -> Nothing

-- | Parse a URL segment into a 'RadarProduct' given the requested kind.
--
-- >>> map (parseProduct RadarKindRadar) ["64km", "128KM", "512km-composite", "doppler-wind", "national"]
-- [Just Radar64km,Just Radar128km,Just Radar512kmComposite,Just RadarDopplerWind,Just RadarNational]
--
-- >>> map (parseProduct RadarKindRainfall) ["5m", "1h", "since-9am", "24h"]
-- [Just Rain5m,Just Rain1h,Just RainSince9am,Just Rain24h]
--
-- >>> parseProduct RadarKindRainfall "national"
-- Nothing
--
-- >>> parseProduct RadarKindRadar "5m"
-- Nothing
parseProduct ::
  RadarKind ->
  String ->
  Maybe RadarProduct
parseProduct k s =
  case (k, fmap toLower s) of
    (RadarKindRadar, "64km") -> Just Radar64km
    (RadarKindRadar, "128km") -> Just Radar128km
    (RadarKindRadar, "256km") -> Just Radar256km
    (RadarKindRadar, "512km-composite") -> Just Radar512kmComposite
    (RadarKindRadar, "doppler-wind") -> Just RadarDopplerWind
    (RadarKindRadar, "national") -> Just RadarNational
    (RadarKindRainfall, "5m") -> Just Rain5m
    (RadarKindRainfall, "1h") -> Just Rain1h
    (RadarKindRainfall, "since-9am") -> Just RainSince9am
    (RadarKindRainfall, "24h") -> Just Rain24h
    _ -> Nothing

-- | Suffix character that BOM appends to a base radar id to form the
-- product id for a given 'RadarProduct'.
--
-- 'RadarNational' is a special case whose product id is @IDR00004@ and
-- whose \"suffix\" is therefore not appended to a base site id.
--
-- >>> productSuffix Radar128km
-- "3"
--
-- >>> productSuffix RadarDopplerWind
-- "I"
--
-- >>> productSuffix Rain5m
-- "A"
productSuffix ::
  RadarProduct ->
  String
productSuffix p =
  case p of
    Radar64km -> "4"
    Radar128km -> "3"
    Radar256km -> "2"
    Radar512kmComposite -> "1"
    RadarDopplerWind -> "I"
    RadarNational -> ""
    Rain5m -> "A"
    Rain1h -> "B"
    RainSince9am -> "C"
    Rain24h -> "D"

-- | Build the concrete BOM product id for a given site+product pair.
--
-- >>> productCode "IDR02" Radar128km
-- "IDR023"
--
-- >>> productCode "IDR02" RadarDopplerWind
-- "IDR02I"
--
-- >>> productCode "IDR02" Rain5m
-- "IDR02A"
--
-- 'RadarNational' ignores the site argument:
--
-- >>> productCode "IDR02" RadarNational
-- "IDR00004"
productCode ::
  String ->
  RadarProduct ->
  String
productCode _ RadarNational = "IDR00004"
productCode base p = base <> productSuffix p

-- | Product id whose @radar_transparencies@ folder holds the background,
-- range rings, catchments and locations layers to composite behind a
-- frame of the requested product. BOM only publishes transparencies for
-- the four numeric range products; the doppler-wind product borrows the
-- 64 km transparencies and the rainfall accumulations borrow the 128 km
-- transparencies.
--
-- >>> map (transparenciesCode "IDR02") [Radar64km, Radar128km, Radar256km, Radar512kmComposite]
-- ["IDR024","IDR023","IDR022","IDR021"]
--
-- >>> transparenciesCode "IDR02" RadarDopplerWind
-- "IDR024"
--
-- >>> map (transparenciesCode "IDR02") [Rain5m, Rain1h, RainSince9am, Rain24h]
-- ["IDR023","IDR023","IDR023","IDR023"]
--
-- >>> transparenciesCode "IDR02" RadarNational
-- ""
transparenciesCode ::
  String ->
  RadarProduct ->
  String
transparenciesCode base p =
  case p of
    Radar64km -> base <> "4"
    Radar128km -> base <> "3"
    Radar256km -> base <> "2"
    Radar512kmComposite -> base <> "1"
    RadarDopplerWind -> base <> "4"
    RadarNational -> ""
    Rain5m -> base <> "3"
    Rain1h -> base <> "3"
    RainSince9am -> base <> "3"
    Rain24h -> base <> "3"

-- | Lowercase a string, replace every non-alphanumeric run with a single
-- @\'-\'@, and strip any leading or trailing @\'-\'@.
--
-- >>> slugify "Melbourne"
-- "melbourne"
--
-- >>> slugify "Brisbane (Mt Stapylton)"
-- "brisbane-mt-stapylton"
--
-- >>> slugify "Adelaide (Buckland Park)"
-- "adelaide-buckland-park"
--
-- >>> slugify "NW Tasmania (West Takone)"
-- "nw-tasmania-west-takone"
--
-- >>> slugify "   "
-- ""
slugify ::
  String ->
  String
slugify =
  trimDash . collapseDash . fmap normChar . fmap toLower
 where
  normChar c
    | isAlphaNum c = c
    | otherwise = '-'
  collapseDash = go False
   where
    go _ [] = []
    go seen ('-' : xs)
      | seen = go True xs
      | otherwise = '-' : go True xs
    go _ (c : xs) = c : go False xs
  trimDash = dropWhile (== '-') . reverse . dropWhile (== '-') . reverse

-- | Locate a site in the parsed list by base id (@IDR02@), full 128 km
-- product id (@IDR023@) or slugified name (@melbourne@). Comparison is
-- case-insensitive; the first match wins on ambiguous slugs.
--
-- >>> let ss = [RadarSite "IDR02" "Melbourne" "melbourne", RadarSite "IDR71" "Sydney (Terrey Hills)" "sydney-terrey-hills"]
-- >>> fmap radarSiteBase (findSite "melbourne" ss)
-- Just "IDR02"
--
-- >>> fmap radarSiteBase (findSite "IDR02" ss)
-- Just "IDR02"
--
-- >>> fmap radarSiteBase (findSite "IDR023" ss)
-- Just "IDR02"
--
-- >>> fmap radarSiteBase (findSite "SYDNEY-TERREY-HILLS" ss)
-- Just "IDR71"
--
-- >>> findSite "unknown" ss
-- Nothing
findSite ::
  String ->
  [RadarSite] ->
  Maybe RadarSite
findSite q ss =
  let ql = fmap toLower q
      match s =
        let b = fmap toLower (radarSiteBase s)
         in ql == b
              || ql == b <> "3"
              || ql == radarSiteSlug s
   in case filter match ss of
        (s : _) -> Just s
        [] -> Nothing

-- | Extract radar sites from the parsed HTML of the BOM landing page.
-- The relevant markup looks like
--
-- @
-- \<area shape=\"poly\" coords=\"...\" href=\"/products/IDR023.loop.shtml#skip\" alt =\"Melbourne\" title=\"Melbourne\" /\>
-- @
--
-- Every @\<area\>@ tag whose @href@ points at a @\/products\/IDRxxx3.loop.shtml@
-- URL is treated as a site. Duplicate base ids keep the first occurrence.
--
-- >>> parseRadarSites (parseTags "<area href=\"/products/IDR023.loop.shtml#skip\" title=\"Melbourne\" /><area href=\"/products/IDR713.loop.shtml\" title=\"Sydney\" />")
-- [RadarSite {radarSiteBase = "IDR02", radarSiteName = "Melbourne", radarSiteSlug = "melbourne"},RadarSite {radarSiteBase = "IDR71", radarSiteName = "Sydney", radarSiteSlug = "sydney"}]
--
-- >>> parseRadarSites (parseTags "<area href=\"/foo/bar\" title=\"Nope\" />")
-- []
--
-- >>> parseRadarSites []
-- []
parseRadarSites ::
  [Tag String] ->
  [RadarSite]
parseRadarSites =
  let go seen (TagOpen "area" attrs : rest)
        | Just href <- lookup "href" attrs
        , Just title <- lookup "title" attrs
        , Just base <- extractRadarBase href
        , not (S.member base seen) =
            RadarSite base title (slugify title) : go (S.insert base seen) rest
      go seen (_ : rest) = go seen rest
      go _ [] = []
   in go S.empty

-- | Given the @href@ of an @\<area\>@ tag from the radar landing page,
-- return the base radar id (i.e. the 128 km product code minus its
-- trailing @\'3\'@) if the URL looks like a @\/products\/IDRxxx3.loop.shtml@
-- link.
--
-- >>> extractRadarBase "/products/IDR023.loop.shtml#skip"
-- Just "IDR02"
--
-- >>> extractRadarBase "/products/IDR1123.loop.shtml"
-- Just "IDR112"
--
-- >>> extractRadarBase "/products/IDR024.loop.shtml"
-- Nothing
--
-- >>> extractRadarBase "/foo/bar"
-- Nothing
extractRadarBase ::
  String ->
  Maybe String
extractRadarBase href =
  let noFragment = takeWhile (/= '#') href
      fname = reverse (takeWhile (/= '/') (reverse noFragment))
   in case fname of
        'I' : 'D' : 'R' : rest ->
          let (digits, tail1) = span isDigit rest
              expected = ".loop.shtml"
           in case reverse digits of
                '3' : baseDigits
                  | not (null baseDigits)
                  , expected `isPrefixOf` tail1 ->
                      Just ("IDR" <> reverse baseDigits)
                _ -> Nothing
        _ -> Nothing

-- | Extract every @theImageNames[N] = \"path\"@ frame URL from the JS
-- embedded in a @.loop.shtml@ page. Frames appear in the order they
-- animate.
--
-- >>> parseLoopFrames "theImageNames = new Array();\ntheImageNames[0] = \"/radar/IDR023.T.202608240034.png\";\ntheImageNames[1] = \"/radar/IDR023.T.202608240039.png\";\n"
-- ["/radar/IDR023.T.202608240034.png","/radar/IDR023.T.202608240039.png"]
--
-- >>> parseLoopFrames "no frames here"
-- []
--
-- Empty and malformed lines are skipped:
--
-- >>> parseLoopFrames "theImageNames[0] = "
-- []
parseLoopFrames ::
  String ->
  [String]
parseLoopFrames src =
  [ url
  | line <- lines src
  , "theImageNames[" `isPrefixOf` dropWhile isSpace line
  , Just url <- [extractQuoted line]
  ]

-- | Extract the first double-quoted substring from a line, or 'Nothing'
-- if the line does not contain a matching pair of double quotes.
--
-- >>> extractQuoted "foo = \"hello world\";"
-- Just "hello world"
--
-- >>> extractQuoted "no quotes"
-- Nothing
--
-- >>> extractQuoted "one \" only"
-- Nothing
extractQuoted ::
  String ->
  Maybe String
extractQuoted s =
  case dropWhile (/= '"') s of
    '"' : rest ->
      case break (== '"') rest of
        (body, '"' : _) -> Just body
        _ -> Nothing
    _ -> Nothing

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

-- | Fetch a radar image.
--
-- The @site@ argument is required for every product except 'RadarNational',
-- for which it must be 'Nothing' (national has no site). The site string
-- is matched against the base id, full 128 km id or slug of the sites
-- advertised on the BOM landing page.
--
-- 'RadarSingle' returns BOM's own @\/radar\/\<PRODUCT\>.gif@ image
-- verbatim. 'RadarLoop' fetches every frame referenced by the site's
-- @.loop.shtml@ page, composites each on top of the site's background
-- and overlay transparencies, and returns them as an animated GIF.
--
-- >>> :t getRadar
-- getRadar
--   :: RadarPeriod
--      -> RadarKind
--      -> Maybe String
--      -> RadarProduct
--      -> IO (Either RadarError RadarImage)
getRadar ::
  RadarPeriod ->
  RadarKind ->
  Maybe String ->
  RadarProduct ->
  IO (Either RadarError RadarImage)
getRadar period _ maybeSite product' =
  case product' of
    RadarNational ->
      case period of
        RadarSingle -> pure (Left RadarNationalNoSingle)
        RadarLoop -> fetchLoop "" product'
    _ ->
      case maybeSite of
        Nothing -> fetchSites >>= \case
          Left err -> pure (Left err)
          Right ss -> pure (Left (RadarUnknownSite "" ss))
        Just siteIn ->
          fetchSites >>= \case
            Left err -> pure (Left err)
            Right ss ->
              case findSite siteIn ss of
                Nothing -> pure (Left (RadarUnknownSite siteIn ss))
                Just site ->
                  case period of
                    RadarSingle -> fetchSingle (radarSiteBase site) product'
                    RadarLoop -> fetchLoop (radarSiteBase site) product'

-- | Fetch and parse the BOM radar landing page for its list of sites.
fetchSites ::
  IO (Either RadarError [RadarSite])
fetchSites =
  let url = "https://reg.bom.gov.au/australia/radar/"
   in httpGet "radar landing page" url >>= \case
        Left err -> pure (Left err)
        Right body ->
          case parseRadarSites (parseTags (BLC.unpack body)) of
            [] -> pure (Left RadarSitesParseError)
            xs -> pure (Right xs)

-- | Fetch BOM's own single-frame @.gif@ for a product.
fetchSingle ::
  String ->
  RadarProduct ->
  IO (Either RadarError RadarImage)
fetchSingle base p =
  let pid = productCode base p
      url = "https://reg.bom.gov.au/radar/" <> pid <> ".gif"
   in httpGet pid url >>= \case
        Left err -> pure (Left err)
        Right body -> pure (Right (RadarImage "image/gif" body))

-- | Fetch every frame of a loop, composite each on top of the site's
-- background and overlay transparencies, and assemble the result as an
-- animated GIF.
fetchLoop ::
  String ->
  RadarProduct ->
  IO (Either RadarError RadarImage)
fetchLoop base p =
  let pid = productCode base p
      loopUrl =
        case p of
          RadarNational ->
            "https://reg.bom.gov.au/products/national_radar_sat.loop.shtml"
          _ ->
            "https://reg.bom.gov.au/products/" <> pid <> ".loop.shtml"
   in httpGet (pid <> ".loop.shtml") loopUrl >>= \case
        Left err -> pure (Left err)
        Right body ->
          case parseLoopFrames (BLC.unpack body) of
            [] -> pure (Left (RadarLoopFramesParseError pid))
            urls ->
              fetchLayers base p >>= \case
                Left err -> pure (Left err)
                Right base' ->
                  fetchFrames urls >>= \case
                    Left err -> pure (Left err)
                    Right frames ->
                      case buildAnimatedGif base' frames of
                        Left msg -> pure (Left (RadarGifEncodeError msg))
                        Right bs -> pure (Right (RadarImage "image/gif" bs))

-- | Fetch and compose the static base layers (background + range or
-- catchments + locations) that sit behind every animated frame. For the
-- national product no transparencies are published; we fall back to a
-- solid white 512x512 background.
fetchLayers ::
  String ->
  RadarProduct ->
  IO (Either RadarError (Image PixelRGB8))
fetchLayers _ RadarNational =
  pure (Right (solidRGB 512 512 (PixelRGB8 255 255 255)))
fetchLayers base p =
  let tid = transparenciesCode base p
      transUrl layer =
        "https://reg.bom.gov.au/products/radar_transparencies/"
          <> tid
          <> "."
          <> layer
          <> ".png"
      overlayLayers =
        case p of
          Rain5m -> ["catchments", "locations"]
          Rain1h -> ["catchments", "locations"]
          RainSince9am -> ["catchments", "locations"]
          Rain24h -> ["catchments", "locations"]
          _ -> ["range", "locations"]
   in httpGet (tid <> ".background.png") (transUrl "background") >>= \case
        Left err -> pure (Left err)
        Right bgBytes ->
          case decodeAsRGB (tid <> ".background.png") bgBytes of
            Left err -> pure (Left err)
            Right bg ->
              fetchOptionalOverlays tid overlayLayers >>= \case
                Left err -> pure (Left err)
                Right overlays -> pure (Right (foldl' composeOverRGB bg overlays))

-- | Fetch a list of overlay transparencies. Missing (404) layers are
-- silently skipped so that the compose pipeline degrades gracefully when
-- a site's optional layers are not published.
fetchOptionalOverlays ::
  String ->
  [String] ->
  IO (Either RadarError [Image PixelRGBA8])
fetchOptionalOverlays _ [] = pure (Right [])
fetchOptionalOverlays tid (layer : rest) =
  let url =
        "https://reg.bom.gov.au/products/radar_transparencies/"
          <> tid
          <> "."
          <> layer
          <> ".png"
   in httpGetOptional (tid <> "." <> layer <> ".png") url >>= \case
        Left err -> pure (Left err)
        Right Nothing -> fetchOptionalOverlays tid rest
        Right (Just bs) ->
          case decodeAsRGBA (tid <> "." <> layer <> ".png") bs of
            Left err -> pure (Left err)
            Right img ->
              fetchOptionalOverlays tid rest >>= \case
                Left err -> pure (Left err)
                Right imgs -> pure (Right (img : imgs))

-- | Fetch and decode every frame in a loop as RGBA.
fetchFrames ::
  [String] ->
  IO (Either RadarError [Image PixelRGBA8])
fetchFrames [] = pure (Right [])
fetchFrames (path : rest) =
  let url = "https://reg.bom.gov.au" <> path
      label = reverse (takeWhile (/= '/') (reverse path))
   in httpGet label url >>= \case
        Left err -> pure (Left err)
        Right bs ->
          case decodeAsRGBA label bs of
            Left err -> pure (Left err)
            Right img ->
              fetchFrames rest >>= \case
                Left err -> pure (Left err)
                Right imgs -> pure (Right (img : imgs))

-- | Compose each frame in turn onto the shared base and encode all the
-- resulting frames as one animated GIF.
buildAnimatedGif ::
  Image PixelRGB8 ->
  [Image PixelRGBA8] ->
  Either String BL.ByteString
buildAnimatedGif base frames =
  let composed = fmap (composeOverRGB base) frames
      indexed = fmap (palettize defaultPaletteOptions) composed
      delay = 25 :: GifDelay
      gifFrames = fmap (mkGifFrame delay) indexed
   in encodeGifImages LoopingForever gifFrames

-- | Repackage a @(index-image, palette)@ pair as the tuple that
-- 'encodeGifImages' expects.
mkGifFrame ::
  GifDelay ->
  (Image Pixel8, Palette) ->
  (Palette, GifDelay, Image Pixel8)
mkGifFrame d (idx, pal) = (pal, d, idx)

-- | Straight-alpha compose an RGBA overlay onto an opaque RGB base.
-- Missing overlay pixels (if the overlay is smaller than the base) are
-- treated as fully transparent.
composeOverRGB ::
  Image PixelRGB8 ->
  Image PixelRGBA8 ->
  Image PixelRGB8
composeOverRGB base overlay =
  let w = imageWidth base
      h = imageHeight base
      ow = imageWidth overlay
      oh = imageHeight overlay
      readOverlay x y
        | x < ow && y < oh = pixelAt overlay x y
        | otherwise = PixelRGBA8 0 0 0 0
   in generateImage
        ( \x y ->
            blendOverRGB (readOverlay x y) (pixelAt base x y)
        )
        w
        h

-- | Blend one RGBA source pixel over an opaque RGB destination pixel.
--
-- >>> blendOverRGB (PixelRGBA8 255 0 0 0) (PixelRGB8 0 0 255)
-- PixelRGB8 0 0 255
--
-- >>> blendOverRGB (PixelRGBA8 255 0 0 255) (PixelRGB8 0 0 255)
-- PixelRGB8 255 0 0
--
-- Half-opaque red over blue gives a mid purple:
--
-- >>> blendOverRGB (PixelRGBA8 255 0 0 128) (PixelRGB8 0 0 255)
-- PixelRGB8 128 0 127
blendOverRGB ::
  PixelRGBA8 ->
  PixelRGB8 ->
  PixelRGB8
blendOverRGB (PixelRGBA8 sr sg sb sa) (PixelRGB8 dr dg db) =
  let mix c d =
        let c' = fromIntegral c :: Int
            d' = fromIntegral d :: Int
            a' = fromIntegral sa :: Int
         in fromIntegral ((c' * a' + d' * (255 - a')) `div` 255)
   in PixelRGB8 (mix sr dr) (mix sg dg) (mix sb db)

-- | Solid-colour RGB image of the given dimensions.
solidRGB ::
  Int ->
  Int ->
  PixelRGB8 ->
  Image PixelRGB8
solidRGB w h c = generateImage (\_ _ -> c) w h

-- | Decode a lazy 'BL.ByteString' as a PNG and convert to RGBA.
decodeAsRGBA ::
  String ->
  BL.ByteString ->
  Either RadarError (Image PixelRGBA8)
decodeAsRGBA src bs =
  case decodePng (BL.toStrict bs) of
    Left msg -> Left (RadarImageDecodeError src msg)
    Right dyn -> Right (convertRGBA8 (dyn :: DynamicImage))

-- | Decode a lazy 'BL.ByteString' as a PNG and convert to RGB. Alpha is
-- discarded; transparent pixels take the palette's zero colour.
decodeAsRGB ::
  String ->
  BL.ByteString ->
  Either RadarError (Image PixelRGB8)
decodeAsRGB src bs =
  case decodePng (BL.toStrict bs) of
    Left msg -> Left (RadarImageDecodeError src msg)
    Right dyn -> Right (convertRGB8 (dyn :: DynamicImage))

-- | Wrap a Wreq GET call, catching 'HttpException's and returning them
-- as a 'RadarHttpError' tagged with the given source label.
httpGet ::
  String ->
  String ->
  IO (Either RadarError BL.ByteString)
httpGet src url =
  catch
    (fmap (Right . (^. responseBody)) (getWith bomOptions url))
    (\e -> pure (Left (RadarHttpError src (show (e :: HttpException)))))

-- | Like 'httpGet' but yields @Right Nothing@ on any HTTP failure, so
-- optional overlay transparencies that a site does not publish don't
-- abort the whole pipeline.
httpGetOptional ::
  String ->
  String ->
  IO (Either RadarError (Maybe BL.ByteString))
httpGetOptional _ url =
  catch
    (fmap (Right . Just . (^. responseBody)) (getWith bomOptions url))
    (\(_ :: HttpException) -> pure (Right Nothing))

