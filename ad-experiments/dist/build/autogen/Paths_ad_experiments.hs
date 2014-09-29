module Paths_ad_experiments (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jtobin/projects/openbrain/probably-baysig/ad-experiments/.cabal-sandbox/bin"
libdir     = "/Users/jtobin/projects/openbrain/probably-baysig/ad-experiments/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.3/ad-experiments-0.1.0.0"
datadir    = "/Users/jtobin/projects/openbrain/probably-baysig/ad-experiments/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/ad-experiments-0.1.0.0"
libexecdir = "/Users/jtobin/projects/openbrain/probably-baysig/ad-experiments/.cabal-sandbox/libexec"
sysconfdir = "/Users/jtobin/projects/openbrain/probably-baysig/ad-experiments/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ad_experiments_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ad_experiments_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ad_experiments_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ad_experiments_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ad_experiments_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
