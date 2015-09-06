module Main where

import Data.Monoid (mempty)
import Foreign.Cppop.Generator.Main (run)
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std (mod_std)
import Foreign.Cppop.Generator.Std.String (c_string)
import Foreign.Cppop.Generator.Std.Vector (tc_vector)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- | Our @main@ (a) ensures that creating an 'Interface' didn't cause any
-- errors, and (b) calls the Cppop generator main function, 'run'.
main :: IO ()
main = case interfaceResult of
  Left errorMsg -> do
    putStrLn $ "Error initializing interface: " ++ errorMsg
    exitFailure
  Right iface -> do
    args <- getArgs
    run [iface] args
    return ()

-- | An interface that contains two modules (the @std@ one provided by Cppop,
-- and a custom module that instantiates some templates), and will generate
-- Haskell code in @Foreign.Cppop.Example@.
interfaceResult :: Either String Interface
interfaceResult =
  addInterfaceHaskellModuleBase ["Foreign", "Cppop", "Example"] =<<
  interface "example" modules

modules :: [Module]
modules = [mod_std, instancesModule]

instancesModule :: Module
instancesModule =
  modifyModule' (makeModule "instances" "instances.hpp" "instances.cpp") $
  addModuleExports
  [ ExportClass c_vector_string
  ]

-- | An instantiation of the @std::vector<>@ template, with @std::string@ as its
-- argument.  This class's name is @vectorString@, a concatenation of the
-- template's name and the suffix given here.
c_vector_string :: Class
c_vector_string = instantiateClassTemplate' tc_vector "String" [TObj c_string] [] mempty
