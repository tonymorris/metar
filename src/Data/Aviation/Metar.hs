{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Data.Aviation.Metar(
  getNOAAMETAR
, runMETAR
) where

import Control.Applicative(pure)
import Control.Category((.))
import Control.Exception(catch)
import Control.Lens(view, _Wrapped, (&), (.~), (^.))
import Control.Monad(Monad((>>=)))
import Data.Aviation.Metar.METARResult(METARResult(ConnErrorResult, ParseErrorResult, METARResultValue))
import Data.Aviation.Metar.METARResultT(METARResultT(METARResultT))
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char(toUpper)
import Data.Either(Either(Left, Right))
import Data.Function(($))
import Data.Functor(fmap)
import Data.Maybe(Maybe(Nothing, Just))
import Data.String(String)
import Data.Semigroup((<>))
import Network.HTTP.Client(HttpException)
import Network.Stream(ConnError(ErrorMisc))
import Network.Wreq(getWith, defaults, headers, Options, responseBody)
import qualified Network.Wreq as Wreq(Response)
import Prelude(show)
import System.IO(IO, hPutStrLn, putStrLn, stderr)

getNOAAMETAR ::
  String
  -> METARResultT IO String
getNOAAMETAR =
  let options ::
        Options
      options =
        defaults & headers .~
          [
            (
              "Host"
            , "tgftp.nws.noaa.gov"
            )
          , (
              "User-Agent"
            , "tonymorris/metar"
            )
          , (
              "Accept"
            , "*/*"
            )
          , (
              "Accept-Language"
            , "en-US,en;q=0.5"
            )
          , (
              "Accept-Encoding"
            , "text/html"
            )
          , (
              "Connection"
            , "keep-alive"
            )
          , (
              "Pragma"
            , "no-cache"
            )
          , (
              "Cache-Control"
            , "no-cache"
            )
          , (
              "DNT"
            , "1"
            )
          ]
      request xxxx =
        catch (fmap Right (getWith options ("https://tgftp.nws.noaa.gov/data/observations/metar/stations/" <> fmap toUpper xxxx <> ".TXT")))
          (\e ->  let e' :: HttpException
                      e' = e
                  in pure . Left . ErrorMisc . show $ e')
      respMETAR ::
        Wreq.Response ByteString
        -> Maybe String
      respMETAR r =
        case BS.lines (r ^. responseBody) of
          [_, r'] -> Just (BS.unpack r')
          _ -> Nothing
  in METARResultT . fmap (\result -> case result of
        Left e -> ConnErrorResult e
        Right response -> case respMETAR response of
          Nothing -> ParseErrorResult
          Just metar -> METARResultValue metar) . request

runMETAR ::
  [String]
  -> IO ()
runMETAR x =
  let stderr' =
        hPutStrLn stderr
  in  case x of
        [] ->
          do  putStrLn ("metar version " <> VERSION_metar)
              stderr' "enter an argument (ICAO code)"
        (r:_) ->
          let s = view _Wrapped (getNOAAMETAR r)
          in  s >>= \s' ->
              case s' of
                METARResultValue a ->
                  putStrLn a
                ParseErrorResult ->
                  stderr' ("No METAR for " <> r)
                ConnErrorResult e ->
                  stderr' ("Network connection error " <> show e)
