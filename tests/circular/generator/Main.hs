module Main where

import Foreign.Cppop.Generator.Main (run)
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Test.Circular.Flob (flobModule)
import Foreign.Cppop.Generator.Test.Circular.Flub (flubModule)
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
  interfaceAddHaskellModuleBase ["Foreign", "Cppop", "Test"] =<<
  interface "test" modules

modules :: [Module]
modules = [flobModule, flubModule]
