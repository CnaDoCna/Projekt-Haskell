{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_szachy (
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

bindir     = "C:\\Users\\zuza9\\Documents\\GitHub\\Projekt-Haskell\\.stack-work\\install\\325f83f0\\bin"
libdir     = "C:\\Users\\zuza9\\Documents\\GitHub\\Projekt-Haskell\\.stack-work\\install\\325f83f0\\lib\\x86_64-windows-ghc-8.8.3\\szachy-0.1.0.0-KmlFkKArhmPK4fIARHqVUi-szachy-exe"
dynlibdir  = "C:\\Users\\zuza9\\Documents\\GitHub\\Projekt-Haskell\\.stack-work\\install\\325f83f0\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\zuza9\\Documents\\GitHub\\Projekt-Haskell\\.stack-work\\install\\325f83f0\\share\\x86_64-windows-ghc-8.8.3\\szachy-0.1.0.0"
libexecdir = "C:\\Users\\zuza9\\Documents\\GitHub\\Projekt-Haskell\\.stack-work\\install\\325f83f0\\libexec\\x86_64-windows-ghc-8.8.3\\szachy-0.1.0.0"
sysconfdir = "C:\\Users\\zuza9\\Documents\\GitHub\\Projekt-Haskell\\.stack-work\\install\\325f83f0\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "szachy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "szachy_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "szachy_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "szachy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "szachy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "szachy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
