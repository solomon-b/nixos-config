{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_xmonad_solomon (
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/solomon/.cabal/bin"
libdir     = "/home/solomon/.cabal/lib/x86_64-linux-ghc-8.10.7/xmonad-solomon-0.1.0.0-inplace-xmonad-solomon"
dynlibdir  = "/home/solomon/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/solomon/.cabal/share/x86_64-linux-ghc-8.10.7/xmonad-solomon-0.1.0.0"
libexecdir = "/home/solomon/.cabal/libexec/x86_64-linux-ghc-8.10.7/xmonad-solomon-0.1.0.0"
sysconfdir = "/home/solomon/.cabal/etc"

getBinDir     = catchIO (getEnv "xmonad_solomon_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "xmonad_solomon_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "xmonad_solomon_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "xmonad_solomon_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonad_solomon_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmonad_solomon_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir ++ fname
  | otherwise                  = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
