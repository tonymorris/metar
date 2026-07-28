{-# OPTIONS_GHC -Wall #-}

-- | A single failure during METAR lookup, tagged with the source (which BOM
-- state or which upstream API) and either a connection failure or a parse-level
-- reason.
module Data.Aviation.Metar.METARError (
  METARError (..),
  connError,
  parseError,
) where

import Network.Stream (ConnError)

-- $setup
-- >>> import Data.Aviation.Metar.METARError
-- >>> import Network.Stream

-- | Description of one thing that went wrong at one source.
--
-- >>> connError "NOAA" (ErrorMisc "boom")
-- ConnErrorAt "NOAA" (ErrorMisc "boom")
-- >>> parseError "New-South-Wales" "not found"
-- ParseErrorAt "New-South-Wales" "not found"
data METARError
  = -- | Network failure at the named source.
    ConnErrorAt String ConnError
  | -- | Non-network failure at the named source, e.g. HTTP 404 or ICAO
    -- absent from the response.
    ParseErrorAt String String
  deriving (Eq, Show)

-- | Build a 'ConnErrorAt'.
--
-- >>> connError "BOM" (ErrorMisc "response timeout")
-- ConnErrorAt "BOM" (ErrorMisc "response timeout")
connError ::
  String ->
  ConnError ->
  METARError
connError =
  ConnErrorAt

-- | Build a 'ParseErrorAt'.
--
-- >>> parseError "NOAA" "HTTP 404"
-- ParseErrorAt "NOAA" "HTTP 404"
parseError ::
  String ->
  String ->
  METARError
parseError =
  ParseErrorAt
