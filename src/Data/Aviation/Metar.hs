{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Metar where

-- https://gist.github.com/tonymorris/d069c12946c2a41b2b33cf6dc436c075

import Network.HTTP
import Network.Stream
import Network.URI
import Data.Semigroup
import Prelude
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup

request ::
  String
  -> Request String
request yxxx =
  let reqBody =
        "keyword=" <> yxxx <> "&type=search&page=TAF"
      in  setHeaders
        (
          setRequestBody
            (
              mkRequest
                POST
                (URI "http" (Just (URIAuth "" "www.bom.gov.au" "")) "/aviation/php/process.php" "" "")
            )
            ("application/x-www-form-urlencoded", reqBody)
        )
        [
          Header HdrHost                        "www.bom.gov.au"
        , Header HdrUserAgent                   "tonymorris/metar"
        , Header HdrAccept                      "*/*"
        , Header HdrAcceptLanguage              "en-US,en;q=0.5"
        , Header HdrAcceptEncoding              "text/html"
        , Header HdrReferer                     "http://www.bom.gov.au/aviation/forecasts/taf/"
        , Header HdrConnection                  "keep-alive"
        , Header HdrContentType                 "application/x-www-form-urlencoded"
        , Header HdrContentLength               (show (length reqBody))
        , Header HdrCookie                      "check=ok; bom_meteye_windspeed_units_knots=yes"
        , Header HdrPragma                      "no-cache"
        , Header HdrCacheControl                "no-cache"
        , Header (HdrCustom "DNT")              "1"
        , Header (HdrCustom "X-Requested-With") "XMLHttpRequest"
        ]

data TAFResponse =
  TAFResponse
    String -- title
    [String] -- TAF
    [String] -- METAR
  deriving (Eq, Ord, Show)

{-

[
  TagBranch "h3" []
    [TagLeaf (TagText "* BRISBANE YBBN")]
, TagBranch "p" [("class","product")]
    [
      TagLeaf (TagText "TAF YBBN 250507Z 2506/2612")
    , TagBranch "br" [] []
    , TagLeaf (TagText "05010KT 9999 FEW040")
    , TagBranch "br" [] []
    , TagLeaf (TagText "FM250800 05008KT 9999 -SHRA SCT040")
    , TagBranch "br" [] []
    , TagLeaf (TagText "FM251200 33007KT 9999 SCT020")
    , TagBranch "br" [] []
    , TagLeaf (TagText "FM252300 01012KT CAVOK")
    , TagBranch "br" [] []
    , TagLeaf (TagText "FM260700 13014KT 9999 SCT020")
    , TagBranch "br" [] []
    , TagLeaf (TagText "PROB30 TEMPO 2508/2512 VRB20G35KT 2000 TSRA BKN012 SCT040CB")
    , TagBranch "br" [] []
    , TagLeaf (TagText "RMK")
    , TagBranch "br" [] []
    , TagLeaf (TagText "T 24 22 21 20 Q 1016 1017 1016 1015")
    ]
  , TagBranch "p" [("class","product")]
      [
        TagLeaf (TagText "TTF METAR YBBN 250600Z 04011KT 9999 FEW035 25/20 Q1015")
      , TagBranch "br" [] []
      , TagLeaf (TagText "RMK RF00.0/000.0 HAZE")
      , TagBranch "br" [] []
      , TagLeaf (TagText "NOSIG")
      ]
  , TagBranch "p" []
      [
        TagBranch "strong" []
          [
            TagLeaf (TagText "* Note:")
          ]
          , TagLeaf (TagText " The latest TTF automatically supersedes the TAF for the 3 hr validity of the TTF, unless otherwise specified in the TTF.")
      ]
  ]

-}

mkTAFResponse ::
  [TagTree String]
  -> Maybe TAFResponse
mkTAFResponse (TagBranch "h3" [] [TagLeaf (TagText title)] : TagBranch "p" [("class","product")] tafs : TagBranch "p" [("class","product")] metars:_) =
  let tagTexts q =
        q >>= \r ->
          case r of
            TagLeaf (TagText v) ->
              [v]
            _ ->
              []
  in  Just (TAFResponse title (tagTexts tafs) (tagTexts metars))
mkTAFResponse _ =
  Nothing

data TAFResult =
  ConnErrorResult ConnError
  | ParseErrorResult
  | TAFResult TAFResponse
  deriving (Eq, Show)

respTAF ::
  Response String
  -> Maybe TAFResponse
respTAF =
  mkTAFResponse . parseTree . rspBody

getTAF ::
  String
  -> IO TAFResult
getTAF =
  let withResult ::
        Either ConnError (Response String)
        -> TAFResult
      withResult (Left e) =
        ConnErrorResult e
      withResult (Right s) =
        case respTAF s of
          Nothing ->
            ParseErrorResult
          Just z ->
            TAFResult z
  in  fmap withResult . simpleHTTP . request
