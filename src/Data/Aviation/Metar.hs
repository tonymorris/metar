{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Metar where

import Control.Applicative
import Control.Monad
import Network.HTTP
import Network.Stream
import Network.URI
import Data.Semigroup
import Prelude
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup
import Data.Char
import Data.Functor.Apply
import Data.Functor.Alt
import Data.Functor.Bind
import Data.Functor.Extend
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- import Data.Functor.Classes

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

instance Apply TAFResult where
  ConnErrorResult e <.> _ =
    ConnErrorResult e
  ParseErrorResult <.> _ =
    ParseErrorResult
  TAFResult f <.> TAFResult a =
    TAFResult (f a)
  TAFResult _ <.> ConnErrorResult e =
    ConnErrorResult e
  TAFResult _ <.> ParseErrorResult =
    ParseErrorResult

instance Applicative TAFResult where
  pure =
    TAFResult
  (<*>) =
    (<.>)

instance Bind TAFResult where
  ConnErrorResult e >>- _ =
    ConnErrorResult e
  ParseErrorResult >>- _ =
    ParseErrorResult
  TAFResult a >>- f =
    f a

instance Monad TAFResult where
  return =
    pure
  (>>=) =
    (>>-)

instance Foldable TAFResult where
  foldr f z (TAFResult a) =
    f a z
  foldr _ z (ConnErrorResult _ ) =
    z
  foldr _ z ParseErrorResult =
    z

instance Traversable TAFResult where
  traverse f (TAFResult a) =
    TAFResult <$> f a
  traverse _ (ConnErrorResult e) =
    pure (ConnErrorResult e)
  traverse _ ParseErrorResult =
    pure ParseErrorResult

instance Alt TAFResult where
  TAFResult a <!> _ =
    TAFResult a
  ConnErrorResult _ <!> x =
    x
  ParseErrorResult <!> x =
    x

instance Extend TAFResult where
  duplicated (TAFResult a) =
    TAFResult (TAFResult a)
  duplicated (ConnErrorResult e) =
    ConnErrorResult e
  duplicated ParseErrorResult =
    ParseErrorResult

instance Semigroup (TAFResult a) where
  (<>) =
    (<!>)

newtype TAFResultT f a =
  TAFResultT
    (f (TAFResult a))

instance Functor f => Functor (TAFResultT f) where
  fmap f (TAFResultT x) =
    TAFResultT (fmap (fmap f) x)

instance Monad f => Apply (TAFResultT f) where
  (<.>) =
    ap

instance Monad f => Applicative (TAFResultT f) where
  pure =
    TAFResultT . pure . pure
  (<*>) =
    ap

instance Monad f => Bind (TAFResultT f) where
  (>>-) =
    (>>=)

instance Monad f => Monad (TAFResultT f) where
  return =
    pure
  TAFResultT x >>= f =
    TAFResultT
      (
        x >>= \x' ->
        case x' of
          TAFResult x'' ->
            let TAFResultT r = f x''
            in  r
          ConnErrorResult e ->
            pure (ConnErrorResult e)
          ParseErrorResult ->
            pure ParseErrorResult
      )

instance Foldable f => Foldable (TAFResultT f) where
  foldr f z (TAFResultT x) =
    foldr (\a b -> foldr f b a) z x

instance Traversable f => Traversable (TAFResultT f) where
  traverse f (TAFResultT x) =
    TAFResultT <$> traverse (traverse f) x

instance Monad f => Alt (TAFResultT f) where
  TAFResultT x <!> TAFResultT y =
    TAFResultT
      (
        x >>= \x' ->
        case x' of
          TAFResult x'' ->
            pure (TAFResult x'')
          ConnErrorResult _ ->
            y
          ParseErrorResult ->
            y
      )

instance MonadIO f => MonadIO (TAFResultT f) where
  liftIO =
    TAFResultT . liftIO . fmap pure

instance MonadTrans TAFResultT where
  lift =
    TAFResultT . fmap pure

instance Monad f => Semigroup (TAFResultT f a) where
  (<>) =
    (<!>)

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

data BOMTAFResponse =
  BOMTAFResponse
    String -- title
    [String] -- TAF
    [String] -- METAR
  deriving (Eq, Ord, Show)

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
getNOAATAF ::
  String
  -> TAFResultT IO String
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
  in TAFResultT . fmap (withResult respTAF) . simpleHTTP . request
