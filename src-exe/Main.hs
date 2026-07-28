{-# OPTIONS_GHC -Wall #-}

module Main (
  main,
) where

import Data.Aviation.Metar (runMETAR)
import System.Environment (getArgs)

main ::
  IO ()
main =
  getArgs >>= runMETAR
