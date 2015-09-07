module Foreign.Cppop.Generator.Main (
  Action (..),
  run,
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad ((<=<), unless, when)
import Data.Foldable (forM_)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Foreign.Cppop.Common (writeFileIfDifferent)
import qualified Foreign.Cppop.Generator.Language.Cpp as Cpp
import qualified Foreign.Cppop.Generator.Language.Haskell as Haskell
import Foreign.Cppop.Generator.Spec
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)

data Action =
  ListInterfaces
  | GenCpp FilePath
  | GenHaskell FilePath

data AppState = AppState
  { appInterfaces :: [Interface]
  , appCurrentInterface :: Interface
  , appCaches :: Caches
  }

initialAppState :: [Interface] -> AppState
initialAppState ifaces = AppState
  { appInterfaces = ifaces
  , appCurrentInterface = head ifaces
  , appCaches = M.empty
  }

type Caches = Map String InterfaceCache

data InterfaceCache = InterfaceCache
  { cacheInterface :: Interface
  , generatedCpp :: Maybe Cpp.Generation
  , generatedHaskell :: Maybe Haskell.Generation
  }

emptyCache :: Interface -> InterfaceCache
emptyCache iface = InterfaceCache iface Nothing Nothing

getGeneratedCpp :: InterfaceCache -> IO (InterfaceCache, Either String Cpp.Generation)
getGeneratedCpp cache = case generatedCpp cache of
  Just gen -> return (cache, Right gen)
  _ -> case Cpp.generate $ cacheInterface cache of
    l@(Left _) -> return (cache, l)
    r@(Right gen) -> return (cache { generatedCpp = Just gen }, r)

getGeneratedHaskell :: InterfaceCache -> IO (InterfaceCache, Either String Haskell.Generation)
getGeneratedHaskell cache = case generatedHaskell cache of
  Just gen -> return (cache, Right gen)
  _ -> case Haskell.generate $ cacheInterface cache of
    l@(Left _) -> return (cache, l)
    r@(Right gen) -> return (cache { generatedHaskell = Just gen }, r)

run :: [Interface] -> [String] -> IO [Action]
run interfaces args = do
  stateVar <- newMVar $ initialAppState interfaces
  when (null args) $ do
    putStrLn "This is a Cppop interface generator.  Use --help for options."
    exitSuccess
  when ("--help" `elem` args) $ usage stateVar >> exitSuccess
  processArgs stateVar args

usage :: MVar AppState -> IO ()
usage stateVar = do
  interfaceNames <- map interfaceName <$> getInterfaces stateVar
  mapM_ putStrLn
    [ "Cppop binding generator"
    , ""
    , "Interfaces: " ++ intercalate ", " interfaceNames
    , ""
    , "Supported options:"
    , "  --help                      Displays this menu."
    , "  --list-interfaces           Lists the interfaces compiled into this binary."
    , "  --gen-cpp <outdir>          Generate C++ bindings in a directory."
    , "  --gen-hs <outdir>           Generate Haskell bindings under the given"
    , "                              top-level source directory."
    ]

processArgs :: MVar AppState -> [String] -> IO [Action]
processArgs stateVar args =
  case args of
    [] -> return []

    "--list-interfaces":rest -> do
      listInterfaces stateVar
      (ListInterfaces:) <$> processArgs stateVar rest

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
          putStrLn $ "--gen-cpp: Failed to generate: " ++ errorMsg
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
          putStrLn $ "--gen-hs: Failed to generate: " ++ errorMsg
          exitFailure
        Right gen -> do
          forM_ (M.toList $ Haskell.generatedFiles gen) $
            uncurry $ writeGeneratedFile baseDir
          (GenHaskell baseDir:) <$> processArgs stateVar rest

    arg:_ -> do
      putStrLn $ "Invalid option or missing argument for " ++ arg ++ "."
      exitFailure

writeGeneratedFile :: FilePath -> FilePath -> String -> IO ()
writeGeneratedFile baseDir subpath contents = do
  let path = baseDir </> subpath
  createDirectoryIfMissing True $ takeDirectory path
  writeFileIfDifferent path contents

withCurrentCache :: MVar AppState -> (InterfaceCache -> IO (InterfaceCache, a)) -> IO a
withCurrentCache stateVar fn = modifyMVar stateVar $ \state -> do
  let currentInterface = appCurrentInterface state
      name = interfaceName currentInterface
  (cache, result) <- fn $
                     fromMaybe (emptyCache currentInterface) $
                     M.lookup name $
                     appCaches state
  return (state { appCaches = M.insert name cache $ appCaches state }, result)

getCurrentCache :: MVar AppState -> IO InterfaceCache
getCurrentCache stateVar = do
  state <- readMVar stateVar
  let currentInterface = appCurrentInterface state
      name = interfaceName currentInterface
  return $
    fromMaybe (emptyCache currentInterface) $
    M.lookup name $
    appCaches state

listInterfaces :: MVar AppState -> IO ()
listInterfaces = mapM_ (putStrLn . interfaceName) . appInterfaces <=< readMVar

getInterfaces :: MVar AppState -> IO [Interface]
getInterfaces = fmap appInterfaces . readMVar
