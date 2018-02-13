{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hspec (
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
version = Version [2,4,8] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/addition/hspec-2.4.8/.stack-work/install/x86_64-linux/nightly-2018-02-11/8.2.2/bin"
libdir     = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/addition/hspec-2.4.8/.stack-work/install/x86_64-linux/nightly-2018-02-11/8.2.2/lib/x86_64-linux-ghc-8.2.2/hspec-2.4.8-AD8oV31xXDiFndrFhOZAG"
dynlibdir  = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/addition/hspec-2.4.8/.stack-work/install/x86_64-linux/nightly-2018-02-11/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/addition/hspec-2.4.8/.stack-work/install/x86_64-linux/nightly-2018-02-11/8.2.2/share/x86_64-linux-ghc-8.2.2/hspec-2.4.8"
libexecdir = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/addition/hspec-2.4.8/.stack-work/install/x86_64-linux/nightly-2018-02-11/8.2.2/libexec/x86_64-linux-ghc-8.2.2/hspec-2.4.8"
sysconfdir = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/addition/hspec-2.4.8/.stack-work/install/x86_64-linux/nightly-2018-02-11/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hspec_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hspec_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hspec_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hspec_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hspec_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hspec_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
