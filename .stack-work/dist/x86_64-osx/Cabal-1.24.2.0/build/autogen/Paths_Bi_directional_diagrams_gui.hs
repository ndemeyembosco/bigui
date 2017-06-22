{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Bi_directional_diagrams_gui (
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

bindir     = "/Users/boscondemeye/Documents/Bi_directional_diagrams_gui/.stack-work/install/x86_64-osx/lts-8.19/8.0.2/bin"
libdir     = "/Users/boscondemeye/Documents/Bi_directional_diagrams_gui/.stack-work/install/x86_64-osx/lts-8.19/8.0.2/lib/x86_64-osx-ghc-8.0.2/Bi-directional-diagrams-gui-0.1.0.0"
dynlibdir  = "/Users/boscondemeye/Documents/Bi_directional_diagrams_gui/.stack-work/install/x86_64-osx/lts-8.19/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/boscondemeye/Documents/Bi_directional_diagrams_gui/.stack-work/install/x86_64-osx/lts-8.19/8.0.2/share/x86_64-osx-ghc-8.0.2/Bi-directional-diagrams-gui-0.1.0.0"
libexecdir = "/Users/boscondemeye/Documents/Bi_directional_diagrams_gui/.stack-work/install/x86_64-osx/lts-8.19/8.0.2/libexec"
sysconfdir = "/Users/boscondemeye/Documents/Bi_directional_diagrams_gui/.stack-work/install/x86_64-osx/lts-8.19/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Bi_directional_diagrams_gui_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Bi_directional_diagrams_gui_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Bi_directional_diagrams_gui_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Bi_directional_diagrams_gui_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Bi_directional_diagrams_gui_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Bi_directional_diagrams_gui_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
