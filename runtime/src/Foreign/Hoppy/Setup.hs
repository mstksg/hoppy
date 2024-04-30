-- This file is part of Hoppy.
--
-- Copyright 2015-2024 Bryan Gardiner <bog@khumba.net>
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
--   'ProjectConfig'
--   { generatorExecutableName = \"foobar-generator\"
--   , cppPackageName = \"foobar-cpp\"
--   , cppPackagedSourcesLocation = Nothing
--   , cppSourcesDir = 'GenerateInAutogenDir' \"\"
--   , hsSourcesDir = 'GenerateInAutogenDir' \"\"
--   }
-- @
--
-- The Haskell gateway uses the same code, except calling 'hsMain' instead of
-- 'cppMain'.  This causes all (C++, Haskell) generated sources to be placed in
-- the \"autogen\" directories provided by Cabal, which keeps the source
-- directory clean.  See the documentation of the fields of 'ProjectConfig' for
-- more information on how to set up your project's build process.
--
-- The gateway packages need to set @build-type: Custom@ in their @.cabal@ files
-- to use these setup files.
module Foreign.Hoppy.Setup (
  ProjectConfig (..),
  GenerateLocation (..),
  combinedMain,
  combinedUserHooks,
  cppMain,
  cppUserHooks,
  hsMain,
  hsUserHooks,
  ) where

import Control.Monad (unless, when)
import Data.List (isInfixOf)
import qualified Data.Map as M
import Distribution.InstalledPackageInfo (libraryDirs)
import qualified Distribution.ModuleName as ModuleName
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Package (mkPackageName)
#else
import Distribution.Package (PackageName (PackageName))
#endif
import Distribution.PackageDescription (
  HookedBuildInfo,
  PackageDescription,
  autogenModules,
  emptyBuildInfo,
  extraLibDirs,
  )
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
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
  replVerbosity,
  testVerbosity,
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
import Distribution.Types.ComponentName (ComponentName (CLibName))
#if MIN_VERSION_Cabal(3,0,0)
import Distribution.Types.LibraryName (LibraryName (LMainLibName))
#endif
import Distribution.Types.LocalBuildInfo (componentNameCLBIs)
import Distribution.Verbosity (Verbosity, normal)
import Foreign.Hoppy.Generator.Language.Haskell (getModuleName)
import Foreign.Hoppy.Generator.Main (run)
import Foreign.Hoppy.Generator.Spec (Interface, interfaceModules)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)

-- | Configuration parameters for a project using Hoppy.
data ProjectConfig = ProjectConfig
  { interfaceResult :: Either String Interface
    -- ^ The interface to run the generator with.  This result is returned from
    -- @Foreign.Hoppy.Generator.Spec.interface@ and some may have failed; the
    -- string is a message indicating the problem.

  , cppPackageName :: String
    -- ^ The name of the C++ gateway package.
    --
    -- When using separate Cabal packages for the C++ and Haskell gateway
    -- packages, this must be nonempty.  When using a single combined gateway
    -- package with 'combinedMain', this must be empty.

  , cppPackagedSourcesLocation :: Maybe FilePath
    -- ^ If the C++ gateway package includes C++ files needed for compliation
    -- (either sources or headers), then this should point to the base directory
    -- holding these files, relative to the root of the project.  The project
    -- root itself may be specified with @Just \"\"@.
    --
    -- When present, this is passed to the C++ package's makefile in the
    -- environment variable @HOPPY_PKG_CPP_DIR@.  A value of @Just \"\"@ is
    -- passed with @HOPPY_PKG_CPP_DIR@ set to the base directory of the Cabal
    -- package (equivalent to @Just \".\"@).
    --
    -- This is also added automatically as a system include path (i.e. @gcc -I@)
    -- for the C++ compiler when compiling the test program for enum
    -- autodetection.

  , cppGeneratedSourcesLocation :: GenerateLocation
    -- ^ Specifies the directory where C++ sources will be generated.
    --
    -- This is passed to the C++ package's makefile in the environment variable
    -- @HOPPY_CPP_GEN_DIR@.
  }

-- | Where to generate sources for binding packages.
data GenerateLocation =
  GenerateInAutogenDir FilePath
  -- ^ Generate sources in the package's autogen directory provided by Cabal.
  -- This is preferrable as it keeps the source directory clean.
  --
  -- Sources are generated below the given @FilePath@ if nonempty, otherwise
  -- sources are generated directly in the autogen directory.
  | GenerateInSourcesDir FilePath
  -- ^ Generate sources in the package's root source directory, i.e. the
  -- directory with the @.cabal@ file.
  --
  -- Sources are generated below the given @FilePath@ if nonempty, otherwise
  -- sources are generated directly in the root directory.

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

-- | A @main@ implementation to be used in the @Setup.hs@ of a single Hoppy
-- binding package that combined both the C++ and Haskell gateway code in one
-- package.
--
-- @combinedMain project = 'defaultMainWithHooks' $ 'combinedUserHooks' project@
combinedMain :: ProjectConfig -> IO ()
combinedMain project = defaultMainWithHooks $ combinedUserHooks project

-- | Cabal user hooks for a combined gateway package.  When overriding
-- overriding fields in the result, be sure to call the previous hook.
--
-- The following hooks are defined:
--
-- - 'postConf': Runs the generator to generate C++ and Haskell sources.
combinedUserHooks :: ProjectConfig -> UserHooks
combinedUserHooks project =
  simpleUserHooks
  { postConf = \args flags pkgDesc localBuildInfo -> do
      let verbosity = fromFlagOrDefault normal $ configVerbosity flags

      when (not $ null $ cppPackageName project) $
        die' verbosity $
        "combinedMain expects an empty cppPackageName, found \"" ++
        cppPackageName project ++ "\"."

      iface <- case interfaceResult project of
        Left errorMsg ->
          die' verbosity $
          "Error initializing interface: " ++ errorMsg
        Right iface -> return iface

      genCpp project verbosity localBuildInfo iface
      genHs verbosity localBuildInfo iface
      postConf simpleUserHooks args flags pkgDesc localBuildInfo
  }

  where genCpp :: ProjectConfig -> Verbosity -> LocalBuildInfo -> Interface -> IO ()
        genCpp project verbosity localBuildInfo iface = do
          (_, cppGenDir) <-
            getAutogenAndCppGenDir project verbosity localBuildInfo
          cppPackagedSourcesDir <- case cppPackagedSourcesLocation project of
            Nothing -> return ""
            Just subpath -> fmap (</> subpath) getCurrentDirectory
          createDirectoryIfMissing True cppGenDir
          createDirectoryIfMissing True $ buildDir localBuildInfo
          _ <- run [iface]
                   [ "--enum-eval-cache-mode", "refresh"
                   , "--enum-eval-cache-path", buildDir localBuildInfo </> enumEvalCacheFileName
                   , "--gen-cpp", cppGenDir, cppPackagedSourcesDir
                   ]
          return ()

        genHs :: Verbosity -> LocalBuildInfo -> Interface -> IO ()
        genHs verbosity localBuildInfo iface = do
          (_, hsGenDir) <- getAutogenAndHsGenDir verbosity localBuildInfo
          createDirectoryIfMissing True hsGenDir
          _ <- run [iface]
                   [ "--enum-eval-cache-mode", "must-exist"
                   , "--enum-eval-cache-path", buildDir localBuildInfo </> enumEvalCacheFileName
                   , "--gen-hs", hsGenDir
                   ]
          return ()

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
      cppBuild project verbosity localBuildInfo

  , copyHook = \pkgDesc localBuildInfo hooks flags -> do
      copyHook simpleUserHooks pkgDesc localBuildInfo hooks flags
      let verbosity = fromFlagOrDefault normal $ copyVerbosity flags
          dest = fromFlagOrDefault NoCopyDest $ copyDest flags
      cppInstall project verbosity pkgDesc localBuildInfo dest

  , instHook = \pkgDesc localBuildInfo hooks flags -> do
      instHook simpleUserHooks pkgDesc localBuildInfo hooks flags
      let verbosity = fromFlagOrDefault normal $ installVerbosity flags
          dest = maybe NoCopyDest CopyTo $
                 flagToMaybe $ installDistPref flags
      cppInstall project verbosity pkgDesc localBuildInfo dest

  , regHook = \pkgDesc localBuildInfo hooks flags -> do
      regHook simpleUserHooks pkgDesc localBuildInfo hooks flags
      let verbosity = fromFlagOrDefault normal $ regVerbosity flags
      cppRegister project verbosity localBuildInfo flags

  , cleanHook = \pkgDesc z hooks flags -> do
      cleanHook simpleUserHooks pkgDesc z hooks flags
      let verbosity = fromFlagOrDefault normal $ cleanVerbosity flags
      cppClean project verbosity
  }

makeProgram :: Program
makeProgram = simpleProgram "make"

defaultLibComponentName :: ComponentName
defaultLibComponentName =
#if MIN_VERSION_Cabal(3,0,0)
  CLibName LMainLibName
#else
  CLibName
#endif

-- | Locates the autogen directory for the library component.
getAutogenDir :: Verbosity -> LocalBuildInfo -> IO FilePath
getAutogenDir verbosity localBuildInfo = do
  let libCLBIs = componentNameCLBIs localBuildInfo defaultLibComponentName
#if MIN_VERSION_Cabal(2,0,0)
      dieFn = die' verbosity
#else
      dieFn = die
#endif

  case libCLBIs of
    [libCLBI] -> return $ autogenComponentModulesDir localBuildInfo libCLBI
    _ ->
      -- TODO Show interface name, and "C++" or "Haskell"?
      dieFn $ concat
      ["Expected one library ComponentLocalBuildInfo, found ",
       show $ length libCLBIs, "."]

getAutogenAndGenDir :: GenerateLocation
                    -> Verbosity
                    -> LocalBuildInfo
                    -> IO (FilePath, FilePath)
getAutogenAndGenDir genLoc verbosity localBuildInfo = do
  autogenDir <- getAutogenDir verbosity localBuildInfo
  case genLoc of
    GenerateInAutogenDir subpath ->
      return (autogenDir, autogenDir </> subpath)
    GenerateInSourcesDir subpath -> do
      curDir <- getCurrentDirectory
      return (autogenDir, curDir </> subpath)

getAutogenAndCppGenDir :: ProjectConfig -> Verbosity -> LocalBuildInfo -> IO (FilePath, FilePath)
getAutogenAndCppGenDir project verbosity localBuildInfo =
  getAutogenAndGenDir (cppGeneratedSourcesLocation project) verbosity localBuildInfo

getAutogenAndHsGenDir :: Verbosity -> LocalBuildInfo -> IO (FilePath, FilePath)
getAutogenAndHsGenDir verbosity localBuildInfo =
  getAutogenAndGenDir (GenerateInAutogenDir "") verbosity localBuildInfo

getCppDirEnvVars :: ProjectConfig -> Verbosity -> LocalBuildInfo -> IO [String]
getCppDirEnvVars project verbosity localBuildInfo = do
  (autogenDir, cppGenDir) <- getAutogenAndCppGenDir project verbosity localBuildInfo
  return $
    [ "HOPPY_AUTOGEN_DIR=" ++ autogenDir
    , "HOPPY_CPP_GEN_DIR=" ++ cppGenDir
    ] ++
    (case cppPackagedSourcesLocation project of
      Just "" -> ["HOPPY_PKG_CPP_DIR=."]
      Just subpath -> ["HOPPY_PKG_CPP_DIR=" ++ subpath]
      Nothing -> [])

cppConfigure :: ProjectConfig -> Verbosity -> LocalBuildInfo -> IO ()
cppConfigure project verbosity localBuildInfo = do
  -- Invoke the generator to create C++ code.
  (_, cppGenDir) <- getAutogenAndCppGenDir project verbosity localBuildInfo
  cppPackagedSourcesDir <- case cppPackagedSourcesLocation project of
    Nothing -> return ""
    Just subpath -> fmap (</> subpath) getCurrentDirectory
  iface <- getInterface project verbosity
  createDirectoryIfMissing True cppGenDir
  createDirectoryIfMissing True $ buildDir localBuildInfo
  _ <- run [iface]
           [ "--enum-eval-cache-mode", "refresh"
           , "--enum-eval-cache-path", buildDir localBuildInfo </> enumEvalCacheFileName
           , "--gen-cpp", cppGenDir, cppPackagedSourcesDir
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

cppBuild :: ProjectConfig -> Verbosity -> LocalBuildInfo -> IO ()
cppBuild project verbosity localBuildInfo = do
  hasMakefile <- doesFileExist "Makefile"
  unless hasMakefile $
#if MIN_VERSION_Cabal(2,0,0)
    die' verbosity
#else
    die
#endif
    "No Makefile found."

  cppDirEnvVars <- getCppDirEnvVars project verbosity localBuildInfo

  let programDb = withPrograms localBuildInfo
  runDbProgram verbosity makeProgram programDb cppDirEnvVars

cppInstall :: ProjectConfig
           -> Verbosity
           -> PackageDescription
           -> LocalBuildInfo
           -> CopyDest
           -> IO ()
cppInstall project verbosity pkgDesc localBuildInfo dest = do
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
  cppDirEnvVars <- getCppDirEnvVars project verbosity localBuildInfo
  runDbProgram verbosity makeProgram programDb $
    ["install", "libdir=" ++ libDir] ++ cppDirEnvVars

  -- We're doing an old-style install, so copy the enum eval cache file from the
  -- build directory to the library directory where the Haskell side of the
  -- bindings will find it.
  let enumEvalCacheFilePath = buildDir localBuildInfo </> enumEvalCacheFileName
  enumEvalCacheExists <- doesFileExist enumEvalCacheFilePath
  when enumEvalCacheExists $
    installOrdinaryFile verbosity
                        enumEvalCacheFilePath
                        (libDir </> enumEvalCacheFileName)

cppRegister :: ProjectConfig -> Verbosity -> LocalBuildInfo -> RegisterFlags -> IO ()
cppRegister project verbosity localBuildInfo flags = do
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
    cppDirEnvVars <- getCppDirEnvVars project verbosity localBuildInfo
    runDbProgram verbosity makeProgram programDb $
      ["install", "libdir=" ++ libDir] ++ cppDirEnvVars

cppClean :: ProjectConfig -> Verbosity -> IO ()
cppClean project verbosity = do
  iface <- getInterface project verbosity
  -- We can remove generated sources if we wrote them to the source directory.
  -- We don't have access to the autogen directory from this hook though.
  -- Although, Cabal ought to remove the autogen directory already when
  -- cleaning...
  case cppGeneratedSourcesLocation project of
    GenerateInAutogenDir _ -> return ()
    GenerateInSourcesDir subpath -> do
      cppPackagedSourcesDir <- case cppPackagedSourcesLocation project of
        Nothing -> return ""
        Just subpath -> fmap (</> subpath) getCurrentDirectory
      _ <- run [iface] ["--clean-cpp", subpath, cppPackagedSourcesDir]
      return ()

  hasMakefile <- doesFileExist "Makefile"
  unless hasMakefile $
#if MIN_VERSION_Cabal(2,0,0)
    die' verbosity
#else
    die
#endif
    "No Makefile found."
  make <- findSystemProgram verbosity "make"
  runProgram verbosity make ["clean"]  -- No Hoppy directories passed in here!

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

  , preBuild = \_ flags -> hsPreHook project (fromFlagOrDefault normal $ buildVerbosity flags)
  , preTest = \_ flags -> hsPreHook project (fromFlagOrDefault normal $ testVerbosity flags)
  , preCopy = \_ flags -> hsPreHook project (fromFlagOrDefault normal $ copyVerbosity flags)
  , preInst = \_ flags -> hsPreHook project (fromFlagOrDefault normal $ installVerbosity flags)
  , preReg = \_ flags -> hsPreHook project (fromFlagOrDefault normal $ regVerbosity flags)
  , preRepl = \_ flags -> hsPreHook project (fromFlagOrDefault normal $ replVerbosity flags)
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
          (_, hsGenDir) <- getAutogenAndHsGenDir verbosity localBuildInfo
          createDirectoryIfMissing True hsGenDir
          _ <- run [iface]
                   [ "--enum-eval-cache-mode", "must-exist"
                   , "--enum-eval-cache-path", libDir </> enumEvalCacheFileName
                   , "--gen-hs", hsGenDir
                   ]
          return ()

hsPreHook :: ProjectConfig -> Verbosity -> IO HookedBuildInfo
hsPreHook project verbosity = do
  iface <- getInterface project verbosity
  let moduleNames =
        map (ModuleName.fromString . getModuleName iface) $
        M.elems (interfaceModules iface)

  libDir <- readFile hsCppLibDirFile

  -- Injected autogenerated modules here works for most commands, however sdist
  -- is not hookable, so autogen-modules should still be written out manually in
  -- your Cabal file!  (See Cabal issue #6180.)
  return (Just emptyBuildInfo
          { autogenModules = moduleNames
          , extraLibDirs = [libDir]
          }
         , [])
