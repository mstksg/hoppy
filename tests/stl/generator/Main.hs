module Main where

import Foreign.Cppop.Generator.Main (run)
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std (mod_std)
import Foreign.Cppop.Generator.Std.String (c_string)
import qualified Foreign.Cppop.Generator.Std.Vector as Vector
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
modules = [mod_std, testModule]

testModule :: Module
testModule =
  modifyModule' (makeModule "stl" "stl.hpp" "stl.cpp") $
  addModuleExports $
  concat
  [ [ ExportClass c_IntBox
    ]
  , Vector.toExports vectorString
  , Vector.toExports vectorIntBox
  ]

-- | This class is deliberately not encodable, in order to ensure that @vector@
-- isn't relying on its value type being encodable.
c_IntBox :: Class
c_IntBox =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeClass (ident "IntBox") Nothing []
  [ mkCtor "new" []
  , mkCtor "newWithValue" [TInt]
  ]
  [ mkConstMethod "get" [] TInt
  , mkMethod "set" [TInt] TVoid
  ]

vectorString :: Vector.Contents
vectorString = Vector.instantiate "String" $ TObj c_string

vectorIntBox :: Vector.Contents
vectorIntBox = Vector.instantiate "IntBox" $ TObj c_IntBox
