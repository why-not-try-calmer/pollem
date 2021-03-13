{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_server (
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

bindir     = "/home/hades/haskell/pollem/server/.stack-work/install/x86_64-linux-tinfo6/f81f17855f2138ad66975a1082ffaf0e6eb06babeebefa6e3839060628c171d4/8.10.4/bin"
libdir     = "/home/hades/haskell/pollem/server/.stack-work/install/x86_64-linux-tinfo6/f81f17855f2138ad66975a1082ffaf0e6eb06babeebefa6e3839060628c171d4/8.10.4/lib/x86_64-linux-ghc-8.10.4/server-0.1.0.0-1anXukjQbKe3KugffK7Iuv-server-exe"
dynlibdir  = "/home/hades/haskell/pollem/server/.stack-work/install/x86_64-linux-tinfo6/f81f17855f2138ad66975a1082ffaf0e6eb06babeebefa6e3839060628c171d4/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/hades/haskell/pollem/server/.stack-work/install/x86_64-linux-tinfo6/f81f17855f2138ad66975a1082ffaf0e6eb06babeebefa6e3839060628c171d4/8.10.4/share/x86_64-linux-ghc-8.10.4/server-0.1.0.0"
libexecdir = "/home/hades/haskell/pollem/server/.stack-work/install/x86_64-linux-tinfo6/f81f17855f2138ad66975a1082ffaf0e6eb06babeebefa6e3839060628c171d4/8.10.4/libexec/x86_64-linux-ghc-8.10.4/server-0.1.0.0"
sysconfdir = "/home/hades/haskell/pollem/server/.stack-work/install/x86_64-linux-tinfo6/f81f17855f2138ad66975a1082ffaf0e6eb06babeebefa6e3839060628c171d4/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "server_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "server_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
