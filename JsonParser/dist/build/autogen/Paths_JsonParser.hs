{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_JsonParser (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\root\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\root\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\JsonParser-0.1.0.0-HYSZyvo5ymI4mrK6a5t9bV"
dynlibdir  = "C:\\Users\\root\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\root\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\JsonParser-0.1.0.0"
libexecdir = "C:\\Users\\root\\AppData\\Roaming\\cabal\\JsonParser-0.1.0.0-HYSZyvo5ymI4mrK6a5t9bV"
sysconfdir = "C:\\Users\\root\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "JsonParser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "JsonParser_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "JsonParser_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "JsonParser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "JsonParser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "JsonParser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
