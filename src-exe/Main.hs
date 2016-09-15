{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.Metar(runMETAR)
import System.Environment(getArgs)
import System.IO(IO)

main ::
  IO ()
main =
  do  a <- getArgs
      runMETAR a
