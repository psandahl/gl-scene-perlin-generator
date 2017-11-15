{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_gl_scene_perlin_generator (
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

bindir     = "/home/patrik/repos/gl-scene-perlin-generator/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/bin"
libdir     = "/home/patrik/repos/gl-scene-perlin-generator/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/lib/x86_64-linux-ghc-8.0.2/gl-scene-perlin-generator-0.1.0.0-HkDSBXuWrYnJX9NGMLkYR3"
dynlibdir  = "/home/patrik/repos/gl-scene-perlin-generator/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/patrik/repos/gl-scene-perlin-generator/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/share/x86_64-linux-ghc-8.0.2/gl-scene-perlin-generator-0.1.0.0"
libexecdir = "/home/patrik/repos/gl-scene-perlin-generator/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/libexec"
sysconfdir = "/home/patrik/repos/gl-scene-perlin-generator/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gl_scene_perlin_generator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gl_scene_perlin_generator_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "gl_scene_perlin_generator_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "gl_scene_perlin_generator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gl_scene_perlin_generator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gl_scene_perlin_generator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
