{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_xmonad_extras (
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
version = Version [0,13,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/Xmonad/xmonad-extras-0.13.2/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/bin"
libdir     = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/Xmonad/xmonad-extras-0.13.2/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/lib/x86_64-linux-ghc-8.2.2/xmonad-extras-0.13.2-KLhGedeHswbGkaI3pCWbsz"
dynlibdir  = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/Xmonad/xmonad-extras-0.13.2/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/Xmonad/xmonad-extras-0.13.2/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/share/x86_64-linux-ghc-8.2.2/xmonad-extras-0.13.2"
libexecdir = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/Xmonad/xmonad-extras-0.13.2/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/libexec/x86_64-linux-ghc-8.2.2/xmonad-extras-0.13.2"
sysconfdir = "/media/Seag_1TB_Daten/Eigene_Dateien_Josef/home_josef_linux/haskell/projects/Xmonad/xmonad-extras-0.13.2/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "xmonad_extras_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xmonad_extras_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "xmonad_extras_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "xmonad_extras_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonad_extras_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmonad_extras_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
