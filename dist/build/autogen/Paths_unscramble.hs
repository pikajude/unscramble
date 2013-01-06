module Paths_unscramble (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,3], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/otters/.dev/Haskell/unscramble/cabal-dev//bin"
libdir     = "/Users/otters/.dev/Haskell/unscramble/cabal-dev//lib/unscramble-0.3/ghc-7.4.2"
datadir    = "/Users/otters/.dev/Haskell/unscramble/cabal-dev//share/unscramble-0.3"
libexecdir = "/Users/otters/.dev/Haskell/unscramble/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "unscramble_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "unscramble_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "unscramble_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "unscramble_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
