{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Metar.BOMTAFResult where

import Control.Lens(makeClassy)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data BOMTAFResponse =
  BOMTAFResponse {
    _bomTitle ::
      String
  , _bomTAF ::
      [String]
  , _bomMETAR ::
      [String]
  } deriving (Eq, Ord, Show)

makeClassy ''BOMTAFResponse
