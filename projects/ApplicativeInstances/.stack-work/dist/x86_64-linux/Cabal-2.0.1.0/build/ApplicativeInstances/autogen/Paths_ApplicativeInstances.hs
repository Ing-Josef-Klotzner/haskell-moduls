{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ApplicativeInstances (
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

bindir     = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/ApplicativeInstances/.stack-work/install/x86_64-linux/lts-11.0/8.2.2/bin"
libdir     = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/ApplicativeInstances/.stack-work/install/x86_64-linux/lts-11.0/8.2.2/lib/x86_64-linux-ghc-8.2.2/ApplicativeInstances-0.1.0.0-5mvMqjKnP3SBfik0lDNy8M-ApplicativeInstances"
dynlibdir  = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/ApplicativeInstances/.stack-work/install/x86_64-linux/lts-11.0/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/ApplicativeInstances/.stack-work/install/x86_64-linux/lts-11.0/8.2.2/share/x86_64-linux-ghc-8.2.2/ApplicativeInstances-0.1.0.0"
libexecdir = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/ApplicativeInstances/.stack-work/install/x86_64-linux/lts-11.0/8.2.2/libexec/x86_64-linux-ghc-8.2.2/ApplicativeInstances-0.1.0.0"
sysconfdir = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/ApplicativeInstances/.stack-work/install/x86_64-linux/lts-11.0/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ApplicativeInstances_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ApplicativeInstances_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ApplicativeInstances_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ApplicativeInstances_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ApplicativeInstances_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ApplicativeInstances_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
