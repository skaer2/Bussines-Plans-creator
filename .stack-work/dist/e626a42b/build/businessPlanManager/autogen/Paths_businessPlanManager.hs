{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_businessPlanManager (
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

bindir     = "C:\\Users\\skaer2\\Documents\\Haskell\\gtkTest\\businessPlanManager\\.stack-work\\install\\b85f993e\\bin"
libdir     = "C:\\Users\\skaer2\\Documents\\Haskell\\gtkTest\\businessPlanManager\\.stack-work\\install\\b85f993e\\lib\\x86_64-windows-ghc-8.6.5\\businessPlanManager-0.1.0.0-3wbb6Vk9Qn52SajIYeUWJI-businessPlanManager"
dynlibdir  = "C:\\Users\\skaer2\\Documents\\Haskell\\gtkTest\\businessPlanManager\\.stack-work\\install\\b85f993e\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\skaer2\\Documents\\Haskell\\gtkTest\\businessPlanManager\\.stack-work\\install\\b85f993e\\share\\x86_64-windows-ghc-8.6.5\\businessPlanManager-0.1.0.0"
libexecdir = "C:\\Users\\skaer2\\Documents\\Haskell\\gtkTest\\businessPlanManager\\.stack-work\\install\\b85f993e\\libexec\\x86_64-windows-ghc-8.6.5\\businessPlanManager-0.1.0.0"
sysconfdir = "C:\\Users\\skaer2\\Documents\\Haskell\\gtkTest\\businessPlanManager\\.stack-work\\install\\b85f993e\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "businessPlanManager_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "businessPlanManager_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "businessPlanManager_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "businessPlanManager_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "businessPlanManager_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "businessPlanManager_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
