-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Main (main) where

import Foreign.Hoppy.Generator.Main (run)
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Comparable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Std (mod_std)
import Foreign.Hoppy.Generator.Std.String (c_string)
import qualified Foreign.Hoppy.Generator.Std.List as List
import qualified Foreign.Hoppy.Generator.Std.Set as Set
import qualified Foreign.Hoppy.Generator.Std.Vector as Vector
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
modules = [mod_std, testModule]

testModule :: Module
testModule =
  modifyModule' (makeModule "stl" "stl.hpp" "stl.cpp") $
  addModuleExports $
  concat
  [ [ ExportClass c_IntBox
    , ExportClass c_IntBoxComparable
    , ExportClass c_IntBoxEquatable
    ]
  , List.toExports listIntBox
  , List.toExports listIntBoxComparable
  , List.toExports listIntBoxEquatable
  , Set.toExports setIntBox
  , Vector.toExports vectorIntBox
  , Vector.toExports vectorIntBoxComparable
  , Vector.toExports vectorIntBoxEquatable
  , Vector.toExports vectorString
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

c_IntBoxComparable :: Class
c_IntBoxComparable =
  addReqIncludes [includeLocal "intbox.hpp"] $
  classAddFeatures [Comparable] $
  makeClass (ident "IntBoxComparable") Nothing [c_IntBox] [] []

c_IntBoxEquatable :: Class
c_IntBoxEquatable =
  addReqIncludes [includeLocal "intbox.hpp"] $
  classAddFeatures [Equatable] $
  makeClass (ident "IntBoxEquatable") Nothing [c_IntBox] [] []

listIntBox :: List.Contents
listIntBox = List.instantiate "IntBox" $ TObj c_IntBox

listIntBoxComparable :: List.Contents
listIntBoxComparable = List.instantiate "IntBoxComparable" $ TObj c_IntBoxComparable

listIntBoxEquatable :: List.Contents
listIntBoxEquatable = List.instantiate "IntBoxEquatable" $ TObj c_IntBoxEquatable

setIntBox :: Set.Contents
setIntBox = Set.instantiate "IntBox" $ TObj c_IntBoxComparable

vectorIntBox :: Vector.Contents
vectorIntBox = Vector.instantiate "IntBox" $ TObj c_IntBox

vectorIntBoxComparable :: Vector.Contents
vectorIntBoxComparable = Vector.instantiate "IntBoxComparable" $ TObj c_IntBox

vectorIntBoxEquatable :: Vector.Contents
vectorIntBoxEquatable = Vector.instantiate "IntBoxEquatable" $ TObj c_IntBox

vectorString :: Vector.Contents
vectorString = Vector.instantiate "String" $ TObj c_string
