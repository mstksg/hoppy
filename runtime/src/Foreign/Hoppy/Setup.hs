-- This file is part of Hoppy.
--
-- Copyright 2015-2021 Bryan Gardiner <bog@khumba.net>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE CPP #-}

-- | Implementations of Cabal setup programs for use in packages of generated
-- bindings.
--
-- Much like the default Setup.hs that Cabal recommends for packages,
--
-- @
-- import "Distribution.Simple"
-- main = 'Distribution.Simple.defaultMain'
-- @
--
-- this module provides simplified configuration of packages for generated
-- bindings.  Suppose you have a project named foobar that is composed of Cabal
-- packages named \"foobar-generator\" for the code generator, \"foobar-cpp\"
-- for the C++ gateway, and \"foobar\" for the Haskell gateway.  The C++ gateway
-- package can use the following code:
--
-- @
-- import "Foreign.Hoppy.Setup" ('ProjectConfig' (..), 'cppMain')
--
-- main =
--   cppMain $
--   ProjectConfig
--   { generatorExecutableName = \"foobar-generator\"
--   , cppPackageName = \"foobar-cpp\"
--   , cppSourcesDir = \"cpp\"
--   , hsSourcesDir = \"src\"
--   }
-- @
--
-- The Haskell gateway uses the same code, except calling 'hsMain' instead of
-- 'cppMain'.  This causes C++ sources to be generated in @foobar-cpp\/cpp@ and
-- (assuming the Haskell gateway is at @foobar\/@) the Haskell sources to be
-- generated in @foobar\/src@.
--
-- The gateway packages need to set @build-type: Custom@ in their @.cabal@ files
-- to use these setup files.
module Foreign.Hoppy.Setup (
  ProjectConfig (..),
  cppMain,
  cppUserHooks,
  hsMain,
  hsUserHooks,
  ) where

import Control.Monad (unless, when)
import Data.List (isInfixOf)
import Distribution.InstalledPackageInfo (libraryDirs)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Package (mkPackageName)
#else
import Distribution.Package (PackageName (PackageName))
#endif
import Distribution.PackageDescription (
  HookedBuildInfo,
  PackageDescription,
  emptyBuildInfo,
  extraLibDirs,
  )
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
  LocalBuildInfo,
  absoluteInstallDirs,
  buildDir,
  installedPkgs,
  libdir,
  withPrograms,
  )
import Distribution.Simple.PackageIndex (lookupPackageName)
import Distribution.Simple.Program (
  runDbProgram,
  runProgram,
  )
import Distribution.Simple.Program.Find (
  ProgramSearchPathEntry (ProgramSearchPathDefault),
  findProgramOnSearchPath,
  )
import Distribution.Simple.Program.Types (
  ConfiguredProgram,
  Program,
  ProgramLocation (FoundOnSystem),
  simpleConfiguredProgram,
  simpleProgram,
  )
import Distribution.Simple.Setup (
  CopyDest (CopyTo, NoCopyDest),
  RegisterFlags,
  buildVerbosity,
  cleanVerbosity,
  configVerbosity,
  copyDest,
  copyVerbosity,
  flagToMaybe,
  fromFlagOrDefault,
  installDistPref,
  installVerbosity,
  regInPlace,
  regVerbosity,
  )
import Distribution.Simple.UserHooks (
  UserHooks (
    buildHook,
    hookedPrograms,
    cleanHook,
    copyHook,
    instHook,
    regHook,
    postConf,
    preBuild,
    preCopy,
    preInst,
    preReg,
    preRepl,
    preTest
    ),
  )
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Simple.Utils (die')
#else
import Distribution.Simple.Utils (die)
#endif
import Distribution.Simple.Utils (installOrdinaryFile)
import Distribution.Verbosity (Verbosity, normal)
import Foreign.Hoppy.Generator.Main (run)
import Foreign.Hoppy.Generator.Spec (Interface)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)

-- | Configuration parameters for a project using Hoppy.
data ProjectConfig = ProjectConfig
  { interfaceResult :: Either String Interface
    -- ^ The interface to run the generator with.  This result is returned from
    -- @Foreign.Hoppy.Generator.Spec.interface@ and some may have failed; the
    -- string is a message indicating the problem.
  , cppPackageName :: String
    -- ^ The name of the C++ gateway package.
  , cppSourcesDir :: FilePath
    -- ^ The directory into which to generate C++ sources, under the C++ gateway
    -- package root.
  , hsSourcesDir :: FilePath
    -- ^ The directory into which to generate Haskell sources, under the Haskell
    -- gateway package root.
  }

-- | The name of the file we'll use to hold the enum evaluation cache.
enumEvalCacheFileName :: FilePath
enumEvalCacheFileName = "hoppy-enum-eval-cache"

-- | Extracts the 'Interface' from a 'ProjectConfig', checking its
-- 'interfaceResult' and aborting the program if the result is unsuccessful.
getInterface :: ProjectConfig -> Verbosity -> IO Interface
getInterface project verbosity = case interfaceResult project of
  Left errorMsg ->
#if MIN_VERSION_Cabal(2,0,0)
    die' verbosity $
#else
    die $
#endif
    "Error initializing interface: " ++ errorMsg
  Right iface -> return iface

-- | A @main@ implementation to be used in the @Setup.hs@ of a C++ gateway
-- package.
--
-- @cppMain project = 'defaultMainWithHooks' $ 'cppUserHooks' project@
cppMain :: ProjectConfig -> IO ()
cppMain project = defaultMainWithHooks $ cppUserHooks project

-- | Cabal user hooks for a C++ gateway package.  When overriding fields in the
-- result, be sure to call the previous hook.
--
-- The following hooks are defined:
--
-- - 'postConf': Runs the generator program to generate C++ sources.  Checks if
-- a @configure@ script exists in the C++ gateway root, and calls it if so
-- (without arguments).
--
-- - 'buildHook': Runs @make@ with no arguments from the C++ gateway root.
--
-- - 'copyHook' and 'instHook': Runs @make install libdir=$libdir@ where
-- @$libdir@ is the directory into which to install the built shared library.
--
-- - 'cleanHook': Removes files created by the generator, then calls @make
-- clean@.
cppUserHooks :: ProjectConfig -> UserHooks
cppUserHooks project =
  simpleUserHooks
  { hookedPrograms = [makeProgram]

  , postConf = \args flags pkgDesc localBuildInfo -> do
      let verbosity = fromFlagOrDefault normal $ configVerbosity flags
      cppConfigure project verbosity localBuildInfo
      postConf simpleUserHooks args flags pkgDesc localBuildInfo

  , buildHook = \pkgDesc localBuildInfo hooks flags -> do
      buildHook simpleUserHooks pkgDesc localBuildInfo hooks flags
      let verbosity = fromFlagOrDefault normal $ buildVerbosity flags
      cppBuild verbosity localBuildInfo

  , copyHook = \pkgDesc localBuildInfo hooks flags -> do
      copyHook simpleUserHooks pkgDesc localBuildInfo hooks flags
      let verbosity = fromFlagOrDefault normal $ copyVerbosity flags
          dest = fromFlagOrDefault NoCopyDest $ copyDest flags
      cppInstall verbosity pkgDesc localBuildInfo dest

  , instHook = \pkgDesc localBuildInfo hooks flags -> do
      instHook simpleUserHooks pkgDesc localBuildInfo hooks flags
      let verbosity = fromFlagOrDefault normal $ installVerbosity flags
          dest = maybe NoCopyDest CopyTo $
                 flagToMaybe $ installDistPref flags
      cppInstall verbosity pkgDesc localBuildInfo dest

  , regHook = \pkgDesc localBuildInfo hooks flags -> do
      regHook simpleUserHooks pkgDesc localBuildInfo hooks flags
      let verbosity = fromFlagOrDefault normal $ regVerbosity flags
      cppRegister verbosity localBuildInfo flags

  , cleanHook = \pkgDesc z hooks flags -> do
      cleanHook simpleUserHooks pkgDesc z hooks flags
      let verbosity = fromFlagOrDefault normal $ cleanVerbosity flags
      cppClean project verbosity
  }

makeProgram :: Program
makeProgram = simpleProgram "make"

cppConfigure :: ProjectConfig -> Verbosity -> LocalBuildInfo -> IO ()
cppConfigure project verbosity localBuildInfo = do
  -- Invoke the generator to create C++ code.
  let sourcesDir = cppSourcesDir project
  iface <- getInterface project verbosity
  createDirectoryIfMissing True sourcesDir
  createDirectoryIfMissing True $ buildDir localBuildInfo
  _ <- run [iface]
           [ "--enum-eval-cache-mode", "refresh"
           , "--enum-eval-cache-path", buildDir localBuildInfo </> enumEvalCacheFileName
           , "--gen-cpp", sourcesDir
           ]

  -- When there is a configure script, then run it.
  maybeConfigureProgram <- findConfigure
  case maybeConfigureProgram of
    Just configureProgram -> runProgram verbosity configureProgram []
    Nothing -> return ()

  where findConfigure :: IO (Maybe ConfiguredProgram)
        findConfigure = do
          hasConfigure <- doesFileExist "configure"
          return $ if hasConfigure
                   then Just $ simpleConfiguredProgram "configure" $ FoundOnSystem "./configure"
                   else Nothing

cppBuild :: Verbosity -> LocalBuildInfo -> IO ()
cppBuild verbosity localBuildInfo = do
  hasMakefile <- doesFileExist "Makefile"
  unless hasMakefile $
#if MIN_VERSION_Cabal(2,0,0)
    die' verbosity
#else
    die
#endif
    "No Makefile found."
  let programDb = withPrograms localBuildInfo
  runDbProgram verbosity makeProgram programDb []

cppInstall :: Verbosity -> PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
cppInstall verbosity pkgDesc localBuildInfo dest = do
  hasMakefile <- doesFileExist "Makefile"
  unless hasMakefile $
#if MIN_VERSION_Cabal(2,0,0)
    die' verbosity
#else
    die
#endif
    "No Makefile found."
  let programDb = withPrograms localBuildInfo
      libDir = libdir $ absoluteInstallDirs pkgDesc localBuildInfo dest
  createDirectoryIfMissing True libDir
  runDbProgram verbosity makeProgram programDb ["install", "libdir=" ++ libDir]

  -- We're doing an old-style install, so copy the enum eval cache file from the
  -- build directory to the library directory where the Haskell side of the
  -- bindings will find it.
  let enumEvalCacheFilePath = buildDir localBuildInfo </> enumEvalCacheFileName
  enumEvalCacheExists <- doesFileExist enumEvalCacheFilePath
  when enumEvalCacheExists $
    installOrdinaryFile verbosity
                        enumEvalCacheFilePath
                        (libDir </> enumEvalCacheFileName)

cppRegister :: Verbosity -> LocalBuildInfo -> RegisterFlags -> IO ()
cppRegister verbosity localBuildInfo flags = do
  hasMakefile <- doesFileExist "Makefile"
  unless hasMakefile $
#if MIN_VERSION_Cabal(2,0,0)
    die' verbosity
#else
    die
#endif
    "No Makefile found."
  when (fromFlagOrDefault False (regInPlace flags)) $ do
    let programDb = withPrograms localBuildInfo
        libDir = buildDir localBuildInfo
    createDirectoryIfMissing True libDir
    runDbProgram verbosity makeProgram programDb ["install", "libdir=" ++ libDir]

cppClean :: ProjectConfig -> Verbosity -> IO ()
cppClean project verbosity = do
  iface <- getInterface project verbosity
  _ <- run [iface] ["--clean-cpp", cppSourcesDir project]

  hasMakefile <- doesFileExist "Makefile"
  unless hasMakefile $
#if MIN_VERSION_Cabal(2,0,0)
    die' verbosity
#else
    die
#endif
    "No Makefile found."
  make <- findSystemProgram verbosity "make"
  runProgram verbosity make ["clean"]

findSystemProgram :: Verbosity -> FilePath -> IO ConfiguredProgram
findSystemProgram verbosity basename = do
  maybePath <-
#if MIN_VERSION_Cabal(1,24,0)
    fmap (fmap fst) $  -- We don't care about failed search paths.
#endif
    findProgramOnSearchPath verbosity [ProgramSearchPathDefault] basename
  case maybePath of
    Just path -> return $ simpleConfiguredProgram basename $ FoundOnSystem path
    Nothing ->
#if MIN_VERSION_Cabal(2,0,0)
      die' verbosity $
#else
      die $
#endif
      "Couldn't find program " ++ show basename ++ "."

-- | A @main@ implementation to be used in the @Setup.hs@ of a Haskell gateway
-- package.
--
-- @hsMain project = 'defaultMainWithHooks' $ 'hsUserHooks' project@
hsMain :: ProjectConfig -> IO ()
hsMain project = defaultMainWithHooks $ hsUserHooks project

-- | Cabal user hooks for a Haskell gateway package.  When overriding fields in
-- the result, be sure to call the previous hook.
--
-- The following hooks are defined:
--
-- - 'postConf': Finds the shared library directory for the installed C++
-- gateway package, and writes this path to a @dist\/build\/hoppy-cpp-libdir@
-- file.  Runs the generator program to generate Haskell sources.
--
-- - 'preBuild', 'preTest', 'preCopy', 'preInst', 'preReg': Reads the C++
-- library directory from @dist\/build\/hoppy-cpp-libdir@ and adds it to the
-- library search path ('extraLibDirs').
--
-- - 'cleanHook': Removes files created by the generator.
hsUserHooks :: ProjectConfig -> UserHooks
hsUserHooks project =
  simpleUserHooks
  { postConf = \args flags pkgDesc localBuildInfo -> do
      postConf simpleUserHooks args flags pkgDesc localBuildInfo
      let verbosity = fromFlagOrDefault normal $ configVerbosity flags
      hsConfigure project verbosity localBuildInfo

  , preBuild = \_ _ -> addLibDir
  , preTest = \_ _ -> addLibDir
  , preCopy = \_ _ -> addLibDir  -- Not sure if necessary, but doesn't hurt.
  , preInst = \_ _ -> addLibDir  -- Not sure if necessary, but doesn't hurt.
  , preReg = \_ _ -> addLibDir  -- Necessary.
  , preRepl = \_ _ -> addLibDir -- Necessary.

  , cleanHook = \pkgDesc z hooks flags -> do
      cleanHook simpleUserHooks pkgDesc z hooks flags
      let verbosity = fromFlagOrDefault normal $ cleanVerbosity flags
      hsClean project verbosity
  }

hsCppLibDirFile :: FilePath
hsCppLibDirFile = "dist/build/hoppy-cpp-libdir"

hsConfigure :: ProjectConfig -> Verbosity -> LocalBuildInfo -> IO ()
hsConfigure project verbosity localBuildInfo = do
  iface <- getInterface project verbosity
  libDir <- lookupCppLibDir
  storeCppLibDir libDir
  generateSources iface libDir

  where lookupCppLibDir = do
          -- Look for an installed -cpp package.
          let packageName = cppPackageName project
          cppPkg <- case lookupPackageName (installedPkgs localBuildInfo) $
#if MIN_VERSION_Cabal(2,0,0)
                         mkPackageName packageName
#else
                         PackageName packageName
#endif
                         of
            [(_, [pkg])] -> return pkg
            results ->
#if MIN_VERSION_Cabal(2,0,0)
              die' verbosity $
#else
              die $
#endif
              "Failed to find a unique " ++ packageName ++
              " installation.  Found " ++ show results ++ "."

          -- Look up the libdir of the package we found.  The filter here is for NixOS,
          -- where libraryDirs includes the library directories of dependencies as well.
          case filter (\x -> packageName `isInfixOf` x) $ libraryDirs cppPkg of
            [libDir] -> return libDir
            libDirs ->
#if MIN_VERSION_Cabal(2,0,0)
              die' verbosity $
#else
              die $
#endif
              "Expected a single library directory for " ++ packageName ++
              ", got " ++ show libDirs ++ "."

        storeCppLibDir libDir = do
          createDirectoryIfMissing True $ takeDirectory hsCppLibDirFile
          writeFile hsCppLibDirFile libDir

        generateSources iface libDir = do
          let sourcesDir = hsSourcesDir project
          createDirectoryIfMissing True sourcesDir
          _ <- run [iface]
                   [ "--enum-eval-cache-mode", "must-exist"
                   , "--enum-eval-cache-path", libDir </> enumEvalCacheFileName
                   , "--gen-hs", sourcesDir
                   ]
          return ()

addLibDir :: IO HookedBuildInfo
addLibDir = do
  libDir <- readFile hsCppLibDirFile
  return (Just emptyBuildInfo {extraLibDirs = [libDir]}, [])

hsClean :: ProjectConfig -> Verbosity -> IO ()
hsClean project verbosity = do
  iface <- getInterface project verbosity
  _ <- run [iface] ["--clean-hs", hsSourcesDir project]
  return ()
