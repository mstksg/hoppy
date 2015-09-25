module Main where

import Data.Monoid (mempty)
import Foreign.Cppop.Generator.Main (run)
import Foreign.Cppop.Generator.Language.Haskell.General (addImports, sayLn)
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Spec.ClassFeature
import Foreign.Cppop.Generator.Spec.Template
import Foreign.Cppop.Generator.Std (mod_std)
import Foreign.Cppop.Generator.Std.String (c_string)
import qualified Foreign.Cppop.Generator.Std.Vector as Vector
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )
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
    run [iface] args
    return ()

interfaceResult :: Either String Interface
interfaceResult =
  addInterfaceHaskellModuleBase ["Foreign", "Cppop", "Test"] =<<
  interface "test" modules

modules :: [Module]
modules = [mod_std, testModule]

testModule :: Module
testModule =
  modifyModule' (makeModule "stl" "stl.hpp" "stl.cpp") $
  addModuleExports $
  Vector.toExports vectorString

vectorString :: Vector.VectorContents
vectorString = Vector.instantiate "String" $ TObj c_string
