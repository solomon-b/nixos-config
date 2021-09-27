{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_xmonad_contrib (
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
version = Version [0,16] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/solomon/.cabal/store/ghc-8.10.7/xmonad-contrib-0.16-832ee4117bf3d05965f5284e9f33384d7d024d21093c36a3a5a53968bc5f8d74/bin"
libdir     = "/home/solomon/.cabal/store/ghc-8.10.7/xmonad-contrib-0.16-832ee4117bf3d05965f5284e9f33384d7d024d21093c36a3a5a53968bc5f8d74/lib"
dynlibdir  = "/home/solomon/.cabal/store/ghc-8.10.7/xmonad-contrib-0.16-832ee4117bf3d05965f5284e9f33384d7d024d21093c36a3a5a53968bc5f8d74/lib"
datadir    = "/home/solomon/.cabal/store/ghc-8.10.7/xmonad-contrib-0.16-832ee4117bf3d05965f5284e9f33384d7d024d21093c36a3a5a53968bc5f8d74/share"
libexecdir = "/home/solomon/.cabal/store/ghc-8.10.7/xmonad-contrib-0.16-832ee4117bf3d05965f5284e9f33384d7d024d21093c36a3a5a53968bc5f8d74/libexec"
sysconfdir = "/home/solomon/.cabal/store/ghc-8.10.7/xmonad-contrib-0.16-832ee4117bf3d05965f5284e9f33384d7d024d21093c36a3a5a53968bc5f8d74/etc"

getBinDir     = catchIO (getEnv "xmonad_contrib_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "xmonad_contrib_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "xmonad_contrib_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "xmonad_contrib_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonad_contrib_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmonad_contrib_sysconfdir") (\_ -> return sysconfdir)




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
