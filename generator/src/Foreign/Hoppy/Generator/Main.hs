-- This file is part of Hoppy.
--
-- Copyright 2015-2020 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}

-- | A driver for a command-line interface to a generator.
--
-- A simple @Main.hs@ for a generator can simply be:
--
-- @
-- import "Foreign.Hoppy.Generator.Main" ('defaultMain')
-- import "Foreign.Hoppy.Generator.Spec" ('ErrorMsg', 'Interface', 'interface')
--
-- interfaceResult :: Either 'ErrorMsg' 'Interface'
-- interfaceResult = 'interface' ...
--
-- main :: IO ()
-- main = 'defaultMain' interfaceResult
-- @
module Foreign.Hoppy.Generator.Main (
  Action (..),
  EnumEvalCacheMode (..),
  defaultMain,
  defaultMain',
  ensureInterfaces,
  run,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Arrow ((&&&))
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Monad ((<=<), forM, unless, when)
import Data.Foldable (forM_)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Foreign.Hoppy.Generator.Common (fromMaybeM, writeFileIfDifferent)
import Foreign.Hoppy.Generator.Hook (internalEvaluateEnumsForInterface)
import qualified Foreign.Hoppy.Generator.Language.Cpp as Cpp
import qualified Foreign.Hoppy.Generator.Language.Cpp.Internal as Cpp
import qualified Foreign.Hoppy.Generator.Language.Haskell.Internal as Haskell
import Foreign.Hoppy.Generator.Spec
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)

-- | Actions that can be requested of the program.
data Action =
    SelectInterface String
    -- ^ Sets the interface that will be used for subsequent actions.
  | ListInterfaces
    -- ^ Lists the interfaces compiled into the generator.
  | ListCppFiles
    -- ^ Lists the generated files in C++ bindings.
  | ListHsFiles
    -- ^ Lists the generated files in Haskell bindings.
  | GenCpp FilePath
    -- ^ Generates C++ wrappers for an interface in the given location.
  | GenHaskell FilePath
    -- ^ Generates Haskell bindings for an interface in the given location.
  | CleanCpp FilePath
    -- ^ Removes the generated files in C++ bindings.
  | CleanHs FilePath
    -- ^ Removes the generated files in Haskell bindings.
  | KeepTempOutputsOnFailure
    -- ^ Instructs the generator to keep on disk any temporary programs or files
    -- created, in case of failure.
  | DumpExtNames
    -- ^ Dumps to stdout information about all external names in the current
    -- interface.
  | DumpEnums
    -- ^ Dumps to stdout information about all enums in the current interface.
  | EnumEvalCachePath (Maybe FilePath)
    -- ^ Specifies the path to a enum evaluation cache file to use.
  | EnumEvalCacheMode EnumEvalCacheMode
    -- ^ Specifies the behaviour with respect to how the enum evaluation cache
    -- cache file is used.

data AppState = AppState
  { appInterfaces :: Map String Interface
  , appCurrentInterfaceName :: String
  , appCaches :: Caches
  , appKeepTempOutputsOnFailure :: Bool
  , appEnumEvalCachePath :: Maybe FilePath
  , appEnumEvalCacheMode :: EnumEvalCacheMode
  }

-- | Controls the behaviour of a generatior with respect to the enum cache file,
-- when a file path provided (@--enum-eval-cache-path@).
--
-- If enum evaluation is required, based on the presence of the cache file and
-- which of these modes is selected, then the compiler will be called and the
-- results will be written to the cache file
--
-- If an enum cache file path is not provided, then this mode is ignored, and
-- enum evaluation is attempted if a generator requires it.
--
-- Change detection is not currently supported.  There is no ability to detect
-- whether the cache file is up to date and contains all of the enum entries for
-- the current state of the enums defined in an interface.  The cache file is
-- meant to be refreshed with 'RefreshEnumCache' when building the C++ binding
-- package, and installed with them so that the Haskell binding package can use
-- 'EnumCacheMustExist'.
data EnumEvalCacheMode =
    RefreshEnumCache
    -- ^ The default.  Ignore the presence of an existing cache file, and
    -- evaluate enums freshly, updating the cache file with new contents.
  | EnumCacheMustExist
    -- ^ Require the cache file to exist.  If it does not, enum evaluation will
    -- not be attempted; the generator will exit unsuccessfully instead.

appCurrentInterface :: AppState -> Interface
appCurrentInterface state =
  let name = appCurrentInterfaceName state
  in case M.lookup name $ appInterfaces state of
       Just iface -> iface
       Nothing ->
         error $
         "Main.appCurrentInterface: Internal error, couldn't find current interface " ++
         show name ++ "."

initialAppState :: [Interface] -> AppState
initialAppState ifaces = AppState
  { appInterfaces = M.fromList $ map (interfaceName &&& id) ifaces
  , appCurrentInterfaceName = interfaceName $ head ifaces
  , appCaches = M.empty
  , appKeepTempOutputsOnFailure = False
  , appEnumEvalCachePath = Nothing
  , appEnumEvalCacheMode = RefreshEnumCache
  }

type Caches = Map String InterfaceCache

data InterfaceCache = InterfaceCache
  { cacheGeneratedCpp :: Maybe Cpp.Generation
  , cacheGeneratedHaskell :: Maybe Haskell.Generation
  , cacheComputedData :: Maybe ComputedInterfaceData
  }

emptyCache :: InterfaceCache
emptyCache = InterfaceCache Nothing Nothing Nothing

getGeneratedCpp ::
  AppState
  -> Interface
  -> InterfaceCache
  -> IO (InterfaceCache, Either String Cpp.Generation)
getGeneratedCpp state iface cache = case cacheGeneratedCpp cache of
  Just gen -> return (cache, Right gen)
  Nothing -> do
    cache' <- generateComputedData state iface cache
    computedData <- flip fromMaybeM (cacheComputedData cache') $ do
      hPutStrLn stderr $
        "getGeneratedCpp: Expected computed data to already exist for " ++ show iface ++ "."
      exitFailure
    case Cpp.generate iface computedData of
      l@(Left _) -> return (cache', l)
      r@(Right gen) -> return (cache' { cacheGeneratedCpp = Just gen }, r)

getGeneratedHaskell ::
  AppState
  -> Interface
  -> InterfaceCache
  -> IO (InterfaceCache, Either String Haskell.Generation)
getGeneratedHaskell state iface cache = case cacheGeneratedHaskell cache of
  Just gen -> return (cache, Right gen)
  Nothing -> do
    cache' <- generateComputedData state iface cache
    computedData <- flip fromMaybeM (cacheComputedData cache') $ do
      hPutStrLn stderr $
        "getGeneratedHaskell: Expected computed data to already exist for " ++ show iface ++ "."
      exitFailure
    case Haskell.generate iface computedData of
      l@(Left _) -> return (cache', l)
      r@(Right gen) -> return (cache' { cacheGeneratedHaskell = Just gen }, r)

-- | Ensures that the cached computed data for an interface been calculated,
-- doing so if it hasn't.
--
-- This ensures that the 'ComputedInterfaceData' for an 'Interface' has been
-- calculated.  This is only computed once for an interface, and is stored in
-- the interface's 'InterfaceCache'.  This function returns the resulting cache
-- with the computed data populated.
generateComputedData :: AppState -> Interface -> InterfaceCache -> IO InterfaceCache
generateComputedData state iface cache = case cacheComputedData cache of
  Just _ -> return cache
  Nothing -> do
    evaluatedEnumMap <- generateEnumData state iface
    return cache
      { cacheComputedData = Just ComputedInterfaceData
        { computedInterfaceName = interfaceName iface
        , evaluatedEnumMap = evaluatedEnumMap
        }
      }

-- | Generates evaluated enum data for storing in the interface cache.
--
-- If there is a cache path provided, and the file exists, then it is loaded and
-- used.  Otherwise, if there is a cache path provided and the file doesn't
-- exist, but a cache path is set to be required ('appEnumEvalCacheRequire'),
-- then generation aborts.  Otherwise, the enum evaluation hook is called to
-- evaluate the enums (see "Foreign.Hoppy.Generator.Hook"), and if a cache path
-- is provided, then the result is serialized and written to the path.
generateEnumData :: AppState -> Interface -> IO (Map ExtName EvaluatedEnumData)
generateEnumData state iface =
  case appEnumEvalCachePath state of
    Nothing -> doEnumEval
    Just path -> do
      cached <- doesFileExist path
      case (appEnumEvalCacheMode state, cached) of
        (RefreshEnumCache, _) -> evalAndWriteFile path
        (EnumCacheMustExist, True) -> useFile path
        (EnumCacheMustExist, False) -> do
          hPutStrLn stderr $
            "generateEnumData: Error, enum evaluation cache expected for " ++
            show iface ++ ", but none found at path '" ++ path ++ "'."
          exitFailure

  where useFile path = deserializeEnumData <$> readFile path
        evalAndWriteFile path = do
          result <- doEnumEval
          writeFileIfDifferent path $ serializeEnumData result
          return result
        doEnumEval = do
          internalEvaluateEnumsForInterface iface
            (appKeepTempOutputsOnFailure state)

serializeEnumData :: Map ExtName EvaluatedEnumData -> String
serializeEnumData = show . M.mapKeys fromExtName

deserializeEnumData :: String -> Map ExtName EvaluatedEnumData
deserializeEnumData = M.mapKeys toExtName . read

-- | This provides a simple @main@ function for a generator.  Define your @main@
-- as:
--
-- @
-- main = defaultMain $ 'interface' ...
-- @
--
-- Refer to 'run' for how to use the command-line interface.  Use 'defaultMain''
-- if you want to include multiple interfaces in your generator.
defaultMain :: Either String Interface -> IO ()
defaultMain interfaceResult = defaultMain' [interfaceResult]

-- | This is a version of 'defaultMain' that accepts multiple interfaces.
defaultMain' :: [Either String Interface] -> IO ()
defaultMain' interfaceResults = do
  interfaces <- ensureInterfaces interfaceResults
  args <- getArgs
  _ <- run interfaces args
  return ()

-- | Ensures that all of the entries in a list of results coming from
-- 'interface' are successful, and returns the list of 'Interface' values.  If
-- any results are unsuccessful, then an error message is printed, and the
-- program exits with an error ('exitFailure').
ensureInterfaces :: [Either String Interface] -> IO [Interface]
ensureInterfaces interfaceResults = forM interfaceResults $ \case
  Left errorMsg -> do
    hPutStrLn stderr $ "Error initializing interface: " ++ errorMsg
    exitFailure
  Right iface -> return iface

-- | @run interfaces args@ runs the driver with the command-line arguments from
-- @args@ against the listed interfaces, and returns the list of actions
-- performed.
--
-- The recognized arguments are listed below.  The exact forms shown are
-- required; the @--long-arg=value@ style is not supported.
--
-- Arguments are processed in the order given, this means that settings must
-- come before action arguments.
--
-- - __@--help@:__ Displays a menu listing the valid commands.
--
-- - __@--list-interfaces@:__ Lists the interfaces compiled into the generator.
--
-- - __@--interface \<iface\>@:__ Sets the interface that will be used for
--   subsequent arguments.
--
-- - __@--gen-cpp \<outdir\>@:__ Generates C++ bindings in the given directory.
--
-- - __@--gen-hs \<outdir\>@:__ Generates Haskell bindings under the given
--   top-level source directory.
--
-- - __@--enum-eval-cache-path \<cachefile\>@:__ Specifies a cache file to use
--   for the results of enum evaluation.  If the cache file already exists, then
--   it may be loaded to save calling the compiler, depending on the cache mode
--   (@--enum-eval-cache-mode@).  Because enum evaluation results are required
--   when generating both the C++ and Haskell interfaces and these are normally
--   separate packages, this allows the C++ package's evaluation work to be
--   shared with the Haskell package.
--
-- - __@--enum-eval-cache-mode \<refresh|must-exist\>@:__
--   Controls the specific behaviour of the generator with respect to the enum
--   evaluation cache file.  See 'EnumEvalCacheMode'.
run :: [Interface] -> [String] -> IO [Action]
run interfaces args = do
  stateVar <- newMVar $ initialAppState interfaces
  when (null args) $ do
    putStrLn "This is a Hoppy interface generator.  Use --help for options."
    putStrLn ""
    putStrLn $ "Interfaces: " ++ unwords (map interfaceName interfaces)
    exitSuccess
  when ("--help" `elem` args) $ usage stateVar >> exitSuccess
  processArgs stateVar args

usage :: MVar AppState -> IO ()
usage stateVar = do
  interfaceNames <- map interfaceName <$> getInterfaces stateVar
  mapM_ putStrLn
    [ "Hoppy binding generator"
    , ""
    , "Arguments: [ option... ] [ action... ]"
    , "  Arguments are processed in the order seen, so put options before the"
    , "  arguments they apply to.  Normally, pass --gen-* last."
    , ""
    , "Interfaces: " ++ intercalate ", " interfaceNames
    , ""
    , "Supported actions:"
    , "  --help                      Displays this menu."
    , "  --list-interfaces           Lists the interfaces compiled into this binary."
    , "  --list-cpp-files            Lists generated file paths in C++ bindings."
    , "  --list-hs-files             Lists generated file paths in Haskell bindings."
    , "  --gen-cpp <outdir>          Generate C++ bindings in a directory."
    , "  --gen-hs <outdir>           Generate Haskell bindings under the given"
    , "                              top-level source directory."
    , "  --clean-cpp <outdir>        Removes generated file paths in C++ bindings."
    , "  --clean-hs <outdir>         Removes generated file paths in Haskell bindings."
    , "  --dump-ext-names            Lists the current interface's external names."
    , "  --dump-enums                Lists the current interface's enum data."
    , ""
    , "Supported options:"
    , "  --interface <iface>         Sets the interface used for subsequent options."
    , "  --keep-temp-outputs-on-failure"
    , "                              Keeps on disk any temporary programs that fail"
    , "                              to build.  Pass this before --gen-* commands."
    , "  --enum-eval-cache-path <path>"
    , "  --enum-eval-cache-mode <refresh|must-exist>"
    , "          Controls the behaviour of the enum evaluation result caching."
    , "          Caching is disabled if no path is given.  With 'refresh', enums"
    , "          are always evaluated freshly and the cache file is updated."
    , "          With 'must-exist', the cache file must already exist."
    ]

processArgs :: MVar AppState -> [String] -> IO [Action]
processArgs stateVar args =
  case args of
    [] -> return []

    "--interface":name:rest -> do
      modifyMVar_ stateVar $ \state ->
        case M.lookup name $ appInterfaces state of
          Nothing -> do
            hPutStrLn stderr $
              "--interface: Interface '" ++ name ++ "' doesn't exist in this generator."
            _ <- exitFailure
            return state
          Just _ -> return state { appCurrentInterfaceName = name }
      (SelectInterface name:) <$> processArgs stateVar rest

    "--list-interfaces":rest -> do
      listInterfaces stateVar
      (ListInterfaces:) <$> processArgs stateVar rest

    "--list-cpp-files":rest -> do
      genResult <- withCurrentCache stateVar getGeneratedCpp
      case genResult of
        Left errorMsg -> do
          hPutStrLn stderr $ "--list-cpp-files: Failed to generate: " ++ errorMsg
          exitFailure
        Right gen -> do
          mapM_ putStrLn $ M.keys $ Cpp.generatedFiles gen
          (ListCppFiles:) <$> processArgs stateVar rest

    "--list-hs-files":rest -> do
      genResult <- withCurrentCache stateVar getGeneratedHaskell
      case genResult of
        Left errorMsg -> do
          hPutStrLn stderr $ "--list-hs-files: Failed to generate: " ++ errorMsg
          exitFailure
        Right gen -> do
          mapM_ putStrLn $ M.keys $ Haskell.generatedFiles gen
          (ListHsFiles:) <$> processArgs stateVar rest

    "--gen-cpp":baseDir:rest -> do
      baseDirExists <- doesDirectoryExist baseDir
      unless baseDirExists $ do
        hPutStrLn stderr $
          "--gen-cpp: Please create this directory so that I can generate files in it: " ++
          baseDir
        exitFailure
      genResult <- withCurrentCache stateVar getGeneratedCpp
      case genResult of
        Left errorMsg -> do
          hPutStrLn stderr $ "--gen-cpp: Failed to generate: " ++ errorMsg
          exitFailure
        Right gen -> do
          forM_ (M.toList $ Cpp.generatedFiles gen) $
            uncurry $ writeGeneratedFile baseDir
          (GenCpp baseDir:) <$> processArgs stateVar rest

    "--gen-hs":baseDir:rest -> do
      baseDirExists <- doesDirectoryExist baseDir
      unless baseDirExists $ do
        hPutStrLn stderr $
          "--gen-hs: Please create this directory so that I can generate files in it: " ++
          baseDir
        exitFailure
      genResult <- withCurrentCache stateVar getGeneratedHaskell
      case genResult of
        Left errorMsg -> do
          hPutStrLn stderr $ "--gen-hs: Failed to generate: " ++ errorMsg
          exitFailure
        Right gen -> do
          forM_ (M.toList $ Haskell.generatedFiles gen) $ \(subpath, contents) ->
            writeGeneratedFile baseDir subpath contents
          (GenHaskell baseDir:) <$> processArgs stateVar rest

    "--clean-cpp":baseDir:rest -> do
      baseDirExists <- doesDirectoryExist baseDir
      when baseDirExists $ do
        genResult <- withCurrentCache stateVar $ getGeneratedCpp $ Just baseDir
        case genResult of
          Left errorMsg -> do
            hPutStrLn stderr $ "--clean-cpp: Failed to evaluate interface: " ++ errorMsg
            exitFailure
          Right gen -> do
            -- TODO Remove empty directories.
            forM_ (M.keys $ Cpp.generatedFiles gen) $ \path ->
              removeFile $ baseDir </> path
      (CleanCpp baseDir:) <$> processArgs stateVar rest

    "--clean-hs":baseDir:rest -> do
      baseDirExists <- doesDirectoryExist baseDir
      when baseDirExists $ do
        genResult <- withCurrentCache stateVar $ getGeneratedHaskell
        case genResult of
          Left errorMsg -> do
            hPutStrLn stderr $ "--clean-hs: Failed to evaluate interface: " ++ errorMsg
            exitFailure
          Right gen -> do
            -- TODO Remove empty directories.
            forM_ (M.keys $ Haskell.generatedFiles gen) $ \path ->
              removeFile $ baseDir </> path
      (CleanHs baseDir:) <$> processArgs stateVar rest

    "--dump-ext-names":rest -> do
      withCurrentCache stateVar $ \_ iface cache -> do
        forM_ (interfaceModules iface) $ \m ->
          forM_ (moduleExports m) $ \export ->
          forM_ (getAllExtNames export) $ \extName ->
          putStrLn $ "extname module=" ++ moduleName m ++ " name=" ++ fromExtName extName
        return (cache, ())
      (DumpExtNames:) <$> processArgs stateVar rest

    "--dump-enums":rest -> do
      withCurrentCache stateVar $ \state iface cache -> do
        cache' <- generateComputedData state iface cache
        computed <- flip fromMaybeM (cacheComputedData cache') $ do
          hPutStrLn stderr $ "--dump-enums expected to have evaluated enum data, but doesn't."
          exitFailure
        let allEvaluatedData = evaluatedEnumMap computed
        forM_ (M.toList allEvaluatedData) $ \(extName, evaluatedData) -> do
          m <- flip fromMaybeM (M.lookup extName $ interfaceNamesToModules iface) $ do
            hPutStrLn stderr $
              "--dump-enums couldn't find module for enum " ++ show extName ++ "."
            exitFailure
          let typeStr =
                Cpp.chunkContents $ Cpp.execChunkWriter $
                Cpp.sayType Nothing $ numType $ evaluatedEnumNumericType evaluatedData
          putStrLn $ "enum name=" ++ fromExtName extName ++ " module=" ++ moduleName m ++
            " type=" ++ typeStr
          forM_ (M.toList $ evaluatedEnumValueMap evaluatedData) $ \(words', number) ->
            putStrLn $ "entry value=" ++ show number ++ " name=" ++ show words'
        return (cache', ())
      (DumpEnums:) <$> processArgs stateVar rest

    "--keep-temp-outputs-on-failure":rest -> do
      modifyMVar_ stateVar $ \state -> return state { appKeepTempOutputsOnFailure = True }
      (KeepTempOutputsOnFailure:) <$> processArgs stateVar rest

    "--enum-eval-cache-path":path:rest -> do
      let path' = if path == "" then Nothing else Just path
      modifyMVar_ stateVar $ \state -> return state { appEnumEvalCachePath = path' }
      (EnumEvalCachePath path':) <$> processArgs stateVar rest

    "--enum-eval-cache-mode":arg:rest -> do
      mode <- case arg of
        "must-exist" -> return EnumCacheMustExist
        "refresh" -> return RefreshEnumCache
        _ -> do
          hPutStrLn stderr $
            "--enum-eval-cache-mode received unexpected argument, got: '" ++ arg ++ "'"
          exitFailure
      modifyMVar_ stateVar $ \state -> return state { appEnumEvalCacheMode = mode }
      (EnumEvalCacheMode mode:) <$> processArgs stateVar rest

    arg:_ -> do
      hPutStrLn stderr $ "Invalid option, or missing argument for '" ++ arg ++ "'."
      exitFailure

writeGeneratedFile :: FilePath -> FilePath -> String -> IO ()
writeGeneratedFile baseDir subpath contents = do
  let path = baseDir </> subpath
  createDirectoryIfMissing True $ takeDirectory path
  writeFileIfDifferent path contents

withCurrentCache ::
  MVar AppState
  -> (AppState -> Interface -> InterfaceCache -> IO (InterfaceCache, a))
  -> IO a
withCurrentCache stateVar fn = modifyMVar stateVar $ \state -> do
  let iface = appCurrentInterface state
      name = interfaceName iface
  let cache = fromMaybe emptyCache $ M.lookup name $ appCaches state
  (cache', result) <- fn state iface cache
  return ( state { appCaches = M.insert name cache' $ appCaches state }
         , result
         )

listInterfaces :: MVar AppState -> IO ()
listInterfaces = mapM_ (putStrLn . interfaceName) <=< getInterfaces

getInterfaces :: MVar AppState -> IO [Interface]
getInterfaces = fmap (M.elems . appInterfaces) . readMVar
