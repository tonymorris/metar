{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Metar where

import Network.HTTP
import Network.Stream
import Network.URI
import Data.Semigroup
import Prelude
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup
import Data.Char

data BOMTAFResponse =
  BOMTAFResponse
    String -- title
    [String] -- TAF
    [String] -- METAR
  deriving (Eq, Ord, Show)

data TAFResult a =
  ConnErrorResult ConnError
  | ParseErrorResult
  | TAFResult a
  deriving (Eq, Show)

instance Functor TAFResult where
  fmap _ (ConnErrorResult e) =
    ConnErrorResult e
  fmap _ ParseErrorResult =
    ParseErrorResult
  fmap f (TAFResult a) =
    TAFResult (f a)

instance Applicative TAFResult where
  pure =
    TAFResult
  ConnErrorResult e <*> _ =
    ConnErrorResult e
  ParseErrorResult <*> _ =
    ParseErrorResult
  TAFResult f <*> TAFResult a =
    TAFResult (f a)
  TAFResult _ <*> ConnErrorResult e =
    ConnErrorResult e
  TAFResult _ <*> ParseErrorResult =
    ParseErrorResult

instance Monad TAFResult where
  return =
    pure
  ConnErrorResult e >>= _ =
    ConnErrorResult e
  ParseErrorResult >>= _ =
    ParseErrorResult
  TAFResult a >>= f =
    f a

newtype TAFResultT f a =
  TAFResultT
    (f (TAFResult a))
-- todo Eq1, Ord1, Show1

withResult ::
  (r -> Maybe a) ->
  Either ConnError r ->
  TAFResult a
withResult _ (Left e) =
  ConnErrorResult e
withResult k (Right s) =
  case k s of
    Nothing ->
      ParseErrorResult
    Just z ->
      TAFResult z

getBOMTAF ::
  String
  -> IO (TAFResult BOMTAFResponse)
getBOMTAF =
  let mkTAFResponse ::
        [TagTree String]
        -> Maybe BOMTAFResponse
      mkTAFResponse (TagBranch "h3" [] [TagLeaf (TagText title)] : TagBranch "p" [("class","product")] tafs : TagBranch "p" [("class","product")] metars:_) =
        let tagTexts q =
              q >>= \r ->
                case r of
                  TagLeaf (TagText v) ->
                    [v]
                  _ ->
                    []
        in  Just (BOMTAFResponse title (tagTexts tafs) (tagTexts metars))
      mkTAFResponse _ =
        Nothing
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
      respTAF ::
        Response String
        -> Maybe BOMTAFResponse
      respTAF =
        mkTAFResponse . parseTree . rspBody
  in  fmap (withResult respTAF) . simpleHTTP . request

-- http://tgftp.nws.noaa.gov/data/observations/metar/stations/xxxx.TXT
getNOAATAF ::
  String
  -> IO (TAFResult String)
getNOAATAF =
  let request ::
        String
        -> Request String
      request xxxx =
        setHeaders
          (
            mkRequest
              GET
              (URI "http" (Just (URIAuth "" "tgftp.nws.noaa.gov" "")) ("data/observations/metar/stations/" <> fmap toUpper xxxx <> ".TXT") "" "")               
          )
          [
            Header HdrHost                        "tgftp.nws.noaa.gov"
          , Header HdrUserAgent                   "tonymorris/metar"
          , Header HdrAccept                      "*/*"
          , Header HdrAcceptLanguage              "en-US,en;q=0.5"
          , Header HdrAcceptEncoding              "text/html"
          , Header HdrConnection                  "keep-alive"
          , Header HdrPragma                      "no-cache"
          , Header HdrCacheControl                "no-cache"
          , Header (HdrCustom "DNT")              "1"
          ]
      respTAF ::
        Response String
        -> Maybe String
      respTAF r =
        case lines (rspBody r) of
          [_, r'] -> Just r'
          _ -> Nothing
  in fmap (withResult respTAF) . simpleHTTP . request
