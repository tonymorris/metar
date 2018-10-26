{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Metar(
  getBOMTAF
, getNOAAMETAR
, getAllMETAR
, getAllTAF
, runMETAR
) where

import Control.Applicative(pure)
import Control.Category((.))
import Control.Lens(view, _Wrapped)
import Control.Monad(Monad((>>=)))
import Data.Aviation.Metar.BOMTAFResult(BOMTAFResponse(BOMTAFResponse), bomMETAR, bomTAF)
import Data.Aviation.Metar.TAFResult(TAFResult(ConnErrorResult, ParseErrorResult, TAFResultValue))
import Data.Aviation.Metar.TAFResultT(TAFResultT(TAFResultT))
import Data.Char(toUpper)
import Data.Either(Either(Left, Right))
import Data.Foldable(length)
import Data.Functor(fmap)
import Data.List(intercalate)
import Data.Maybe(Maybe(Nothing, Just))
import Data.String(String, lines)
import Data.Semigroup((<>))
import Network.HTTP(Request, Response, setHeaders, setRequestBody, mkRequest, RequestMethod(POST, GET), Header(Header), HeaderName(..), rspBody, simpleHTTP)
import Network.Stream(ConnError)
import Network.URI(URI(URI), URIAuth(URIAuth))
import Prelude(show)
import System.IO(IO, hPutStrLn, putStrLn, stderr)
import Text.HTML.TagSoup(Tag(TagText))
import Text.HTML.TagSoup.Tree(TagTree(TagBranch, TagLeaf), parseTree)

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
      TAFResultValue z

getBOMTAF ::
  String
  -> TAFResultT IO BOMTAFResponse
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
  in  TAFResultT . fmap (withResult respTAF) . simpleHTTP . request

-- http://tgftp.nws.noaa.gov/data/observations/metar/stations/xxxx.TXT
getNOAAMETAR ::
  String
  -> TAFResultT IO String
getNOAAMETAR =
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
      respMETAR ::
        Response String
        -> Maybe String
      respMETAR r =
        case lines (rspBody r) of
          [_, r'] -> Just r'
          _ -> Nothing
  in TAFResultT . fmap (withResult respMETAR) . simpleHTTP . request

getAllMETAR ::
  String
  -> TAFResultT IO [String]
getAllMETAR x = 
  fmap (view bomMETAR) (getBOMTAF x) <>
  fmap pure (getNOAAMETAR x)

getAllTAF ::
  String
  -> TAFResultT IO [String]
getAllTAF x = 
  fmap (view bomTAF) (getBOMTAF x)

runMETAR ::
  [String]
  -> IO ()
runMETAR x =
  let stderr' =
        hPutStrLn stderr
  in  case x of
        [] ->
          stderr' "enter an argument (ICAO code)"
        (r:_) ->
          let s = view _Wrapped (fmap (intercalate "\n") (getAllMETAR r))
          in  s >>= \s' ->
              case s' of
                TAFResultValue a ->
                  putStrLn a
                ParseErrorResult ->
                  stderr' ("No METAR for " <> r)
                ConnErrorResult e ->
                  stderr' ("Network connection error " <> show e)
