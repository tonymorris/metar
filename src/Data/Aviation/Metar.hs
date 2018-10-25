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
