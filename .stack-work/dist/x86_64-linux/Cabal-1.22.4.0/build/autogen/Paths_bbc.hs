module Paths_bbc (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ethan/Documents/tufts/comp193/bbc/.stack-work/install/x86_64-linux/lts-3.5/7.10.2/bin"
libdir     = "/home/ethan/Documents/tufts/comp193/bbc/.stack-work/install/x86_64-linux/lts-3.5/7.10.2/lib/x86_64-linux-ghc-7.10.2/bbc-0.1.0.0-21PylIqlSd3FBm7DRhrkil"
datadir    = "/home/ethan/Documents/tufts/comp193/bbc/.stack-work/install/x86_64-linux/lts-3.5/7.10.2/share/x86_64-linux-ghc-7.10.2/bbc-0.1.0.0"
libexecdir = "/home/ethan/Documents/tufts/comp193/bbc/.stack-work/install/x86_64-linux/lts-3.5/7.10.2/libexec"
sysconfdir = "/home/ethan/Documents/tufts/comp193/bbc/.stack-work/install/x86_64-linux/lts-3.5/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bbc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bbc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bbc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bbc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bbc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
