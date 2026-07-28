{-# OPTIONS_GHC -Wall #-}

{- FOURMOLU_DISABLE -}
-- $setup
-- >>> import Data.Aviation.Metar.Cache
{- FOURMOLU_ENABLE -}

-- | On-disk cache of ICAO → BOM-state associations.
--
-- The cache lives at @$XDG_CACHE_HOME/metar/icao-states.txt@ (usually
-- @~/.cache/metar/icao-states.txt@). One @ICAO STATE@ pair per line.
module Data.Aviation.Metar.Cache (
  readCache,
  mergeCache,
) where

import Control.Exception (IOException, catch)
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, doesFileExist, getXdgDirectory, renameFile)
import System.FilePath (takeDirectory, (</>))

-- | Absolute path to the cache file (under @XdgCache@).
--
-- >>> :t cachePath
-- cachePath :: IO String
cachePath ::
  IO String
cachePath =
  fmap (</> "icao-states.txt") (getXdgDirectory XdgCache "metar")

-- | Discard 'IOException' and return the given fallback.
--
-- >>> :t ignoreIO
-- ignoreIO :: a -> IOException -> IO a
ignoreIO ::
  a ->
  IOException ->
  IO a
ignoreIO a _ =
  pure a

-- | Read all @ICAO STATE@ pairs from the cache file.
-- Returns @[]@ if the file is missing or unreadable.
--
-- >>> :t readCache
-- readCache :: IO [(String, String)]
readCache ::
  IO [(String, String)]
readCache =
  let parseLine ln =
        case words ln of
          [i, s] -> [(i, s)]
          _ -> []
      readIt = do
        p <- cachePath
        ex <- doesFileExist p
        if not ex
          then pure []
          else fmap (concatMap parseLine . lines) (readFile p)
   in catch readIt (ignoreIO [])

-- | Overwrite the cache file with the given entries. Writes atomically via
-- @tmp + rename@. Silently ignores I/O errors.
--
-- >>> :t writeCache
-- writeCache :: [(String, String)] -> IO ()
writeCache ::
  [(String, String)] ->
  IO ()
writeCache entries =
  let body =
        unlines (fmap (\(i, s) -> i <> " " <> s) entries)
      write = do
        p <- cachePath
        createDirectoryIfMissing True (takeDirectory p)
        let tmp = p <> ".tmp"
        writeFile tmp body
        renameFile tmp p
   in catch write (ignoreIO ())

-- | Merge new entries into the existing cache. New ICAOs overwrite any
-- existing state assignment for the same ICAO.
--
-- >>> :t mergeCache
-- mergeCache :: [(String, String)] -> IO ()
mergeCache ::
  [(String, String)] ->
  IO ()
mergeCache new =
  readCache >>= \old ->
    let newIcaos = fmap fst new
        kept = filter (\(i, _) -> i `notElem` newIcaos) old
     in writeCache (new <> kept)
