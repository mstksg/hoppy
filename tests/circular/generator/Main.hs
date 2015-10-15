module Main where

import Foreign.Hoppy.Generator.Main (run)
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Test.Circular.Flob (flobModule)
import Foreign.Hoppy.Generator.Test.Circular.Flub (flubModule)
import System.Environment (getArgs)
import System.Exit (exitFailure)

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = case interfaceResult of
  Left errorMsg -> do
    putStrLn $ "Error initializing interface: " ++ errorMsg
    exitFailure
  Right iface -> do
    args <- getArgs
    _ <- run [iface] args
    return ()

interfaceResult :: Either String Interface
interfaceResult =
  interfaceAddHaskellModuleBase ["Foreign", "Hoppy", "Test"] =<<
  interface "test" modules

modules :: [Module]
modules = [flobModule, flubModule]
