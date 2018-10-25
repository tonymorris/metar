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
import Data.Functor.Classes

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

instance Eq1 TAFResult where
  liftEq _ (ConnErrorResult e1) (ConnErrorResult e2) =
    e1 == e2
  liftEq _ (ConnErrorResult _) ParseErrorResult =
    False
  liftEq _ (ConnErrorResult _) (TAFResult _) =
    False
  liftEq _ ParseErrorResult ParseErrorResult =
    True
  liftEq _ ParseErrorResult (ConnErrorResult _) =
    False
  liftEq _ ParseErrorResult (TAFResult _) =
    False
  liftEq f (TAFResult a) (TAFResult b) =
    f a b
  liftEq _ (TAFResult _) (ConnErrorResult _) =
    False
  liftEq _ (TAFResult _) ParseErrorResult =
    False

instance Show1 TAFResult where
  liftShowsPrec f _ n (TAFResult a) =
    showParen (n > 10) (showString "TAFResult " . f n a)
  liftShowsPrec _ _ n (ConnErrorResult e) =
    showParen (n > 10) (showString "ConnErrorResult " . showsPrec n e)
  liftShowsPrec _ _ n ParseErrorResult =
    showParen (n > 10) (showsPrec n "ParseErrorResult")

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

instance (Eq a, Eq1 f) => Eq (TAFResultT f a) where
  TAFResultT x == TAFResultT y =
    liftEq (==) x y

instance (Show a, Show1 f) => Show (TAFResultT f a) where
  showsPrec n (TAFResultT x) =
    showParen (n > 10) (showString "TAFResultT " . showsPrec1 n x)

instance Eq1 f => Eq1 (TAFResultT f) where
  liftEq f (TAFResultT x) (TAFResultT y) =
    liftEq (liftEq f) x y

instance Show1 f => Show1 (TAFResultT f) where
  liftShowsPrec f g n (TAFResultT x) =
    undefined

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
