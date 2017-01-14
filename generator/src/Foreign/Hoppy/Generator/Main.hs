-- This file is part of Hoppy.
--
-- Copyright 2015-2017 Bryan Gardiner <bog@khumba.net>
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
-- A simple @Main.hs@ for a generator can be:
--
-- > import Foreign.Hoppy.Generator.Main (run)
-- > import Foreign.Hoppy.Generator.Spec (ErrorMsg, Interface, interface, interfaceResult)
-- > import System.Environment (getArgs)
-- > import System.Exit (exitFailure)
-- > import System.IO (hPutStrLn, stderr)
-- >
-- > iface :: Either ErrorMsg Interface
-- > iface = interface ...
-- >
-- > main :: IO ()
-- > main = case interfaceResult of
-- >   Left errorMsg -> do
-- >     hPutStrLn stderr $ "Error initializing interface: " ++ errorMsg
-- >     exitFailure
-- >   Right iface -> do
-- >     args <- getArgs
-- >     _ <- run [iface] args
-- >     return ()
module Foreign.Hoppy.Generator.Main (
  Action (..),
  run,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad ((<=<), unless, when)
import Data.Foldable (forM_)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Foreign.Hoppy.Generator.Common (writeFileIfDifferent)
import qualified Foreign.Hoppy.Generator.Language.Cpp.Internal as Cpp
import qualified Foreign.Hoppy.Generator.Language.Haskell.Internal as Haskell
import Foreign.Hoppy.Generator.Spec
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)

-- | Actions that can be requested of the program.
data Action =
    ListInterfaces
    -- ^ Lists the interfaces compiled into the generator.
  | GenCpp FilePath
    -- ^ Generates C++ wrappers for an interface in the given location.
  | GenHaskell FilePath
    -- ^ Generates Haskell bindings for an interface in the given location.

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

-- | @run interfaces args@ runs the driver with the command-line arguments from
-- @args@ against the listed interfaces, and returns the list of actions
-- performed.
--
-- The recognized arguments are listed below.  The exact forms shown are
-- required; the @--long-arg=value@ style is not supported.
--
-- - __@--help@:__ Displays a menu listing the valid commands.
--
-- - __@--list-interfaces@:__ Lists the interfaces compiled into the generator.
--
-- - __@--gen-cpp \<outdir\>@:__ Generates C++ bindings in the given directory.
--
-- - __@--gen-hs \<outdir\>@:__ Generates Haskell bindings under the given
--   top-level source directory.
run :: [Interface] -> [String] -> IO [Action]
run interfaces args = do
  stateVar <- newMVar $ initialAppState interfaces
  when (null args) $ do
    putStrLn "This is a Hoppy interface generator.  Use --help for options."
    exitSuccess
  when ("--help" `elem` args) $ usage stateVar >> exitSuccess
  processArgs stateVar args

usage :: MVar AppState -> IO ()
usage stateVar = do
  interfaceNames <- map interfaceName <$> getInterfaces stateVar
  mapM_ putStrLn
    [ "Hoppy binding generator"
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

listInterfaces :: MVar AppState -> IO ()
listInterfaces = mapM_ (putStrLn . interfaceName) . appInterfaces <=< readMVar

getInterfaces :: MVar AppState -> IO [Interface]
getInterfaces = fmap appInterfaces . readMVar
