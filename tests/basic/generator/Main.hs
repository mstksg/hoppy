module Main where

import Data.Monoid (mempty)
import Foreign.Cppop.Generator.Main (run)
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std (mod_std)
import Foreign.Cppop.Generator.Std.String (c_string)
import Foreign.Cppop.Generator.Std.Vector (tc_vector)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = case interfaceResult of
  Left errorMsg -> do
    putStrLn $ "Error initializing interface: " ++ errorMsg
    exitFailure
  Right iface -> do
    args <- getArgs
    run [iface] args
    return ()

interfaceResult :: Either String Interface
interfaceResult =
  addInterfaceHaskellModuleBase ["Foreign", "Cppop", "Test"] =<<
  interface "test" modules

modules :: [Module]
modules = [testModule]

testModule :: Module
testModule =
  addReqIncludes [includeLocal "functions.hpp"] $
  modifyModule' (makeModule "basic" "basic.hpp" "basic.cpp") $
  addModuleExports
  [ ExportFn f_piapprox
  , ExportFn f_piapproxNonpure
  , ExportFn f_timesTwo
  ]

f_piapprox :: Function
f_piapprox = makeFn (ident "piapprox") Nothing Pure [] TInt

f_piapproxNonpure :: Function
f_piapproxNonpure =
  makeFn (ident "piapprox") (Just $ toExtName "piapproxNonpure") Nonpure [] TInt

f_timesTwo :: Function
f_timesTwo = makeFn (ident "timesTwo") Nothing Pure [TLong] TLong
