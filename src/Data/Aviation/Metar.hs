{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

{- FOURMOLU_DISABLE -}
-- $setup
-- >>> import Text.HTML.TagSoup (parseTags)
{- FOURMOLU_ENABLE -}

-- | Fetching METAR observations from the BOM (Australia) and NOAA (rest of world).
module Data.Aviation.Metar (
  getBOMMETAR,
  getNOAAMETAR,
  getMETAR,
  runMETAR,
) where

import Control.Exception (catch)
import Control.Lens (view, (&), (.~), (^.), _Wrapped)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Data.Aviation.Metar.Cache (mergeCache, readCache)
import Data.Aviation.Metar.METARError (METARError (ConnErrorAt, ParseErrorAt))
import Data.Aviation.Metar.METARResult (METARResult (METARResultFailure, METARResultValue))
import Data.Aviation.Metar.METARResultT (METARResultT (METARResultT))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (isAsciiUpper, isDigit, isSpace, toUpper)
import Data.Functor.Alt ((<!>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Network.HTTP.Client (HttpException (HttpExceptionRequest, InvalidUrlException), HttpExceptionContent (ConnectionTimeout, ResponseTimeout, StatusCodeException), responseStatus)
import Network.HTTP.Types.Status (statusCode)
import Network.Stream (ConnError (ErrorMisc))
import Network.Wreq (FormParam ((:=)), Options, defaults, getWith, headers, postWith, responseBody)
import qualified Network.Wreq as Wreq (Response)
import System.IO (hPutStrLn, stderr)
import Text.HTML.TagSoup (Tag (TagClose, TagOpen, TagText), parseTags)

-- | The seven BOM state selectors accepted by @process.php@.
--
-- >>> length bomStates
-- 7
-- >>> head bomStates
-- "New-South-Wales"
-- >>> last bomStates
-- "Northern-Territory"
bomStates ::
  [String]
bomStates =
  [ "New-South-Wales"
  , "Victoria"
  , "Queensland"
  , "Western-Australia"
  , "South-Australia"
  , "Tasmania"
  , "Northern-Territory"
  ]

-- | HTTP options used when POSTing to the BOM aviation observations endpoint.
--
-- >>> :t bomOptions
-- bomOptions :: Options
bomOptions ::
  Options
bomOptions =
  defaults
    & headers
      .~ [ ("User-Agent", "tonymorris/metar")
         , ("Accept", "*/*")
         , ("Accept-Language", "en-US,en;q=0.5")
         , ("Referer", "https://www.bom.gov.au/aviation/observations/metar-speci/")
         , ("X-Requested-With", "XMLHttpRequest")
         , ("Cookie", "check=ok")
         ]

-- | Translate an 'HttpException' into a 'METARError' tagged with a source.
-- 404s become 'ParseErrorAt' (upstream doesn't have this ICAO); other
-- failures become 'ConnErrorAt' with a short label.
--
-- >>> :t classifyHttp
-- classifyHttp :: String -> HttpException -> METARError
classifyHttp ::
  String ->
  HttpException ->
  METARError
classifyHttp src (HttpExceptionRequest _ content) =
  case content of
    StatusCodeException resp _ ->
      case statusCode (responseStatus resp) of
        404 -> ParseErrorAt src "HTTP 404"
        n -> ConnErrorAt src (ErrorMisc ("HTTP " <> show n))
    ResponseTimeout ->
      ConnErrorAt src (ErrorMisc "response timeout")
    ConnectionTimeout ->
      ConnErrorAt src (ErrorMisc "connection timeout")
    other ->
      ConnErrorAt src (ErrorMisc (show other))
classifyHttp src (InvalidUrlException url reason) =
  ConnErrorAt src (ErrorMisc ("invalid URL: " ++ url ++ " (" ++ reason ++ ")"))

-- | Build a single-element failure result.
--
-- >>> :t failWith
-- failWith :: METARError -> METARResult a
failWith ::
  METARError ->
  METARResult a
failWith e =
  METARResultFailure (e :| [])

-- | Extract @(ICAO, product-text)@ pairs from a BOM aviation response body.
--
-- >>> extractStations (parseTags "<h3>SYDNEY YSSY 28/07/2026 UTC</h3><p class=\"product\">METAR YSSY 280600Z NCD Q1018</p>")
-- [("YSSY","METAR YSSY 280600Z NCD Q1018")]
-- >>> extractStations (parseTags "<h3>Nothing here</h3><p>ignored</p>")
-- []
-- >>> extractStations []
-- []
extractStations ::
  [Tag String] ->
  [(String, String)]
extractStations tags =
  let go (TagOpen "h3" _ : ts) =
        let (title, ts') = takeText ts
            (product', ts'') = pickProduct ts'
         in case (extractIcao title, product') of
              (Just i, Just p) -> (i, p) : go ts''
              _ -> go ts''
      go (_ : ts) = go ts
      go [] = []
   in go tags

-- | Concatenate 'TagText' contents up to the next @\</h3\>@.
--
-- >>> fst (takeText (parseTags "hello <b>world</b></h3>rest"))
-- "hello world"
-- >>> takeText []
-- ("",[])
takeText ::
  [Tag String] ->
  (String, [Tag String])
takeText =
  let go acc (TagClose "h3" : rest) = (acc, rest)
      go acc (TagText t : rest) = go (acc <> t) rest
      go acc (_ : rest) = go acc rest
      go acc [] = (acc, [])
   in go ""

-- | Find and consume the next @\<p class="product"\>...\</p\>@, returning its
-- text and the tags following. If a fresh @\<h3\>@ appears first we stop and
-- rewind, so the caller can handle it.
--
-- >>> fst (pickProduct (parseTags "<p class=\"product\">METAR YSSY hi</p>rest"))
-- Just "METAR YSSY hi"
-- >>> fst (pickProduct (parseTags "<p>not product</p>"))
-- Nothing
-- >>> fst (pickProduct [])
-- Nothing
pickProduct ::
  [Tag String] ->
  (Maybe String, [Tag String])
pickProduct (TagOpen "p" attrs : rest)
  | lookup "class" attrs == Just "product" =
      let (inside, rest') = break isPClose rest
          txt = concatMap tagText inside
       in (Just txt, drop 1 rest')
 where
  isPClose (TagClose "p") = True
  isPClose _ = False
  tagText (TagText t) = t
  tagText (TagOpen "br" _) = " "
  tagText _ = ""
pickProduct (TagOpen "h3" _ : rest) =
  (Nothing, TagOpen "h3" [] : rest)
pickProduct (_ : rest) =
  pickProduct rest
pickProduct [] =
  (Nothing, [])

-- | Pull the ICAO out of an aerodrome heading. Returns the last four-letter
-- upper-case-alphanumeric token in the title (so multi-word airport names work).
--
-- >>> extractIcao "SYDNEY YSSY 28/07/2026 UTC"
-- Just "YSSY"
-- >>> extractIcao "MOUNT ISA YBMA 28/07/2026 UTC"
-- Just "YBMA"
-- >>> extractIcao "no icao here"
-- Nothing
-- >>> extractIcao ""
-- Nothing
extractIcao ::
  String ->
  Maybe String
extractIcao title =
  let ws = wordsBySpace title
      isIcao s = length s == 4 && all (\c -> isAsciiUpper c || isDigit c) s
      lastIcao (s : rest)
        | isIcao s = case lastIcao rest of
            Just later -> Just later
            Nothing -> Just s
        | otherwise = lastIcao rest
      lastIcao [] = Nothing
   in lastIcao ws

-- | Split on any whitespace. Equivalent to 'Prelude.words'.
--
-- >>> wordsBySpace "hello world"
-- ["hello","world"]
-- >>> wordsBySpace "  spaced   out   "
-- ["spaced","out"]
-- >>> wordsBySpace ""
-- []
wordsBySpace ::
  String ->
  [String]
wordsBySpace s =
  case dropWhile isSpace s of
    "" -> []
    s' -> let (w, s'') = break isSpace s' in w : wordsBySpace s''

-- | Locate the METAR/SPECI product line for a given ICAO in a list of parsed
-- station entries.
--
-- >>> findMETAR "YSSY" [("YSSY", "METAR YSSY 280600Z NCD Q1018")]
-- Just "METAR YSSY 280600Z NCD Q1018"
-- >>> findMETAR "YSSY" [("YSSY", "SPECI YSSY 280601Z 27015G30KT Q1017")]
-- Just "SPECI YSSY 280601Z 27015G30KT Q1017"
-- >>> findMETAR "YSSY" [("YMML", "METAR YMML 280600Z ...")]
-- Nothing
-- >>> findMETAR "YSSY" []
-- Nothing
-- >>> findMETAR "YSSY" [("YSSY", "not a metar")]
-- Nothing
findMETAR ::
  String ->
  [(String, String)] ->
  Maybe String
findMETAR icao =
  let matches (i, p) =
        i == icao && (isPrefixOf "METAR " p || isPrefixOf "SPECI " p)
      go [] = Nothing
      go (s : ss) = if matches s then Just (snd s) else go ss
   in go
 where
  isPrefixOf p s = take (length p) s == p

-- | POST to @process.php@ for one BOM state selector. Network exceptions are
-- caught and returned in the 'Left'.
--
-- >>> :t requestState
-- requestState
--   :: String -> IO (Either HttpException (Wreq.Response ByteString))
requestState ::
  String ->
  IO (Either HttpException (Wreq.Response ByteString))
requestState state =
  let url = "https://www.bom.gov.au/aviation/php/process.php"
      body :: [FormParam]
      body =
        [ "state" := state
        , "page" := ("metar-speci" :: String)
        ]
   in catch (fmap Right (postWith bomOptions url body)) (pure . Left)

-- | Fetch a single state, populate the cache with everything we learned, and
-- return the METAR for the target ICAO (if it was in that state's response).
--
-- >>> :t fetchState
-- fetchState :: String -> String -> METARResultT IO String
fetchState ::
  String ->
  String ->
  METARResultT IO String
fetchState icao state =
  METARResultT $
    requestState state >>= \case
      Left e ->
        pure (failWith (classifyHttp state e))
      Right resp ->
        let stations = extractStations (parseTags (BS.unpack (resp ^. responseBody)))
            entries = fmap (\(i, _) -> (i, state)) stations
         in do
              unless (null entries) (mergeCache entries)
              pure $ case findMETAR icao stations of
                Just m -> METARResultValue m
                Nothing -> failWith (ParseErrorAt state (icao <> " not in response"))

-- | Fetch a METAR from the Bureau of Meteorology. Only @Y*@ ICAOs
-- (Australia's block) are attempted; anything else returns a
-- 'ParseErrorAt' at @"BOM"@ without a network call. Uses the on-disk cache
-- to pick a state first; falls back to scanning all seven states on a miss.
--
-- >>> :t getBOMMETAR
-- getBOMMETAR :: String -> METARResultT IO String
getBOMMETAR ::
  String ->
  METARResultT IO String
getBOMMETAR icao =
  let icao' = fmap toUpper icao
   in case icao' of
        ('Y' : _) ->
          do
            cached <- lift readCache
            case lookup icao' cached of
              Just state ->
                fetchState icao' state <!> scanAllStates icao' [state]
              Nothing ->
                scanAllStates icao' []
        _ ->
          METARResultT (pure (failWith (ParseErrorAt "BOM" (icao' <> " is not an Australian ICAO (Y*)"))))

-- | Try every state (except any already tried) in order.
--
-- >>> :t scanAllStates
-- scanAllStates :: String -> [String] -> METARResultT IO String
scanAllStates ::
  String ->
  [String] ->
  METARResultT IO String
scanAllStates icao exclude =
  let remaining = filter (`notElem` exclude) bomStates
   in case remaining of
        [] -> METARResultT (pure (failWith (ParseErrorAt "BOM" (icao <> " not found in any state"))))
        rs -> foldr1 (<!>) (fmap (fetchState icao) rs)

-- | Fetch a METAR from NOAA's @tgftp.nws.noaa.gov@ station-file endpoint.
--
-- >>> :t getNOAAMETAR
-- getNOAAMETAR :: String -> METARResultT IO String
getNOAAMETAR ::
  String ->
  METARResultT IO String
getNOAAMETAR =
  let options ::
        Options
      options =
        defaults
          & headers
            .~ [ ("Host", "tgftp.nws.noaa.gov")
               , ("User-Agent", "tonymorris/metar")
               , ("Accept", "*/*")
               , ("Accept-Language", "en-US,en;q=0.5")
               , ("Accept-Encoding", "text/html")
               , ("Connection", "keep-alive")
               , ("Pragma", "no-cache")
               , ("Cache-Control", "no-cache")
               , ("DNT", "1")
               ]
      request xxxx =
        catch
          (fmap Right (getWith options ("https://tgftp.nws.noaa.gov/data/observations/metar/stations/" <> fmap toUpper xxxx <> ".TXT")))
          (pure . Left)
      respMETAR ::
        Wreq.Response ByteString ->
        Maybe String
      respMETAR r =
        case BS.lines (r ^. responseBody) of
          [_, r'] -> Just (BS.unpack r')
          _ -> Nothing
   in METARResultT
        . fmap
          ( \case
              Left e -> failWith (classifyHttp "NOAA" e)
              Right response ->
                case respMETAR response of
                  Just m -> METARResultValue m
                  Nothing -> failWith (ParseErrorAt "NOAA" "unexpected response format")
          )
        . request

-- | Fetch a METAR. Try BOM first (which itself returns a 'ParseErrorAt' for
-- non-@Y*@ codes), then fall back to NOAA. Errors from both sources are
-- accumulated in the 'METARResultFailure' list.
--
-- >>> :t getMETAR
-- getMETAR :: String -> METARResultT IO String
getMETAR ::
  String ->
  METARResultT IO String
getMETAR icao =
  getBOMMETAR icao <!> getNOAAMETAR icao

-- | Render one 'METARError' as a human-readable line.
--
-- >>> renderError (ConnErrorAt "NOAA" (ErrorMisc "response timeout"))
-- "NOAA: connection error: response timeout"
-- >>> renderError (ParseErrorAt "New-South-Wales" "not in response")
-- "New-South-Wales: not in response"
renderError ::
  METARError ->
  String
renderError (ConnErrorAt src (ErrorMisc msg)) =
  src <> ": connection error: " <> msg
renderError (ConnErrorAt src e) =
  src <> ": connection error: " <> show e
renderError (ParseErrorAt src msg) =
  src <> ": " <> msg

-- | CLI entry point. Takes the argv list and prints a METAR to @stdout@ or
-- all accumulated errors to @stderr@.
--
-- >>> :t runMETAR
-- runMETAR :: [String] -> IO ()
runMETAR ::
  [String] ->
  IO ()
runMETAR x =
  let stderr' =
        hPutStrLn stderr
   in case x of
        [] ->
          do
            putStrLn ("metar version " <> VERSION_metar)
            stderr' "enter an argument (ICAO code)"
        (r : _) ->
          let s = view _Wrapped (getMETAR r)
           in s >>= \case
                METARResultValue a ->
                  putStrLn a
                METARResultFailure (e :| es) ->
                  do
                    stderr' ("No METAR for " <> r <> ":")
                    mapM_ (stderr' . ("  " <>) . renderError) (e : es)
