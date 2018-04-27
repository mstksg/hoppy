-- This file is part of Hoppy.
--
-- Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
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

module Foreign.Hoppy.Test.Interfaces.Stl (interfaceResult) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Std (ValueConversion (ConvertPtr, ConvertValue), mod_std)
import Foreign.Hoppy.Generator.Std.String (c_string)
import Foreign.Hoppy.Generator.Types
import qualified Foreign.Hoppy.Generator.Std.List as List
import qualified Foreign.Hoppy.Generator.Std.Map as Map
import qualified Foreign.Hoppy.Generator.Std.Pair as Pair
import qualified Foreign.Hoppy.Generator.Std.Set as Set
import qualified Foreign.Hoppy.Generator.Std.UnorderedMap as UnorderedMap
import qualified Foreign.Hoppy.Generator.Std.UnorderedSet as UnorderedSet
import qualified Foreign.Hoppy.Generator.Std.Vector as Vector

{-# ANN module "HLint: ignore Use camelCase" #-}

interfaceResult :: Either String Interface
interfaceResult =
  interfaceAddHaskellModuleBase ["Foreign", "Hoppy", "Test"] =<<
  interface "stl" modules

modules :: [Module]
modules = [mod_std, testModule]

testModule :: Module
testModule =
  moduleModify' (makeModule "stl" "stl.hpp" "stl.cpp") $
  moduleAddExports $
  concat
  [ [ ExportClass c_IntBox
    , ExportClass c_IntBoxComparable
    , ExportClass c_IntBoxEquatable
    ]
  , List.toExports listInt
  , List.toExports listIntBox
  , List.toExports listIntBoxComparable
  , List.toExports listIntBoxEquatable
  , Map.toExports mapInts
  , Map.toExports mapIntBoxes
  , Pair.toExports pairIntBoxes
  , Set.toExports setInt
  , Set.toExports setIntBox
  , UnorderedMap.toExports unorderedMapInts
  , UnorderedMap.toExports unorderedMapIntBoxes
  , UnorderedSet.toExports unorderedSetInt
  , UnorderedSet.toExports unorderedSetIntBox
  , Vector.toExports vectorIntBox
  , Vector.toExports vectorIntBoxComparable
  , Vector.toExports vectorIntBoxEquatable
  , Vector.toExports vectorString
  ]

intBoxInclude :: Include
intBoxInclude = includeLocal "intbox.hpp"

intBoxReqs :: Reqs
intBoxReqs = reqInclude intBoxInclude

-- | This class is deliberately not encodable, in order to ensure that @vector@
-- isn't relying on its value type being encodable.
c_IntBox :: Class
c_IntBox =
  addReqs intBoxReqs $
  classAddFeatures [Assignable, Copyable] $
  makeClass (ident "IntBox") Nothing []
  [ mkCtor "new" []
  , mkCtor "newWithValue" [intT]
  , mkConstMethod "get" [] intT
  , mkMethod "set" [intT] voidT
  ]

c_IntBoxComparable :: Class
c_IntBoxComparable =
  addReqs intBoxReqs $
  classAddFeatures [Assignable, Comparable, Copyable] $
  makeClass (ident "IntBoxComparable") Nothing [c_IntBox] []

c_IntBoxEquatable :: Class
c_IntBoxEquatable =
  addReqs intBoxReqs $
  classAddFeatures [Assignable, Copyable, Equatable] $
  makeClass (ident "IntBoxEquatable") Nothing [c_IntBox] []

listInt :: List.Contents
listInt =
  List.instantiate' "listInt" intT mempty $
  List.defaultOptions { List.optValueConversion = Just ConvertValue }

listIntBox :: List.Contents
listIntBox =
  List.instantiate' "listIntBox" (objT c_IntBox) intBoxReqs $
  List.defaultOptions { List.optValueConversion = Just ConvertPtr }

listIntBoxComparable :: List.Contents
listIntBoxComparable = List.instantiate "listIntBoxComparable" (objT c_IntBoxComparable) intBoxReqs

listIntBoxEquatable :: List.Contents
listIntBoxEquatable = List.instantiate "listIntBoxEquatable" (objT c_IntBoxEquatable) intBoxReqs

mapInts :: Map.Contents
mapInts =
  Map.instantiate' "mapInts" intT intT intBoxReqs $
  Map.defaultOptions
  { Map.optKeyConversion = Just ConvertValue
  , Map.optValueConversion = Just ConvertValue
  }

mapIntBoxes :: Map.Contents
mapIntBoxes =
  Map.instantiate' "mapIntBoxes" (objT c_IntBoxComparable) (objT c_IntBox) intBoxReqs $
  Map.defaultOptions
  { Map.optKeyConversion = Just ConvertPtr
  , Map.optValueConversion = Just ConvertPtr
  }

pairIntBoxes :: Pair.Contents
pairIntBoxes = Pair.instantiate "pairIntBoxes" (objT c_IntBox) (objT c_IntBox) intBoxReqs

setInt :: Set.Contents
setInt =
  Set.instantiate' "setInt" intT intBoxReqs $
  Set.defaultOptions { Set.optValueConversion = Just ConvertValue }

setIntBox :: Set.Contents
setIntBox =
  Set.instantiate' "setIntBox" (objT c_IntBoxComparable) intBoxReqs $
  Set.defaultOptions { Set.optValueConversion = Just ConvertPtr }

unorderedMapInts :: UnorderedMap.Contents
unorderedMapInts =
  UnorderedMap.instantiate' "unorderedMapInts" intT intT intBoxReqs $
  UnorderedMap.defaultOptions
  { UnorderedMap.optKeyConversion = Just ConvertValue
  , UnorderedMap.optValueConversion = Just ConvertValue
  }

unorderedMapIntBoxes :: UnorderedMap.Contents
unorderedMapIntBoxes =
  UnorderedMap.instantiate' "unorderedMapIntBoxes"
                            (objT c_IntBoxEquatable)
                            (objT c_IntBox)
                            intBoxReqs $
  UnorderedMap.defaultOptions
  { UnorderedMap.optKeyConversion = Just ConvertPtr
  , UnorderedMap.optValueConversion = Just ConvertPtr
  }

unorderedSetInt :: UnorderedSet.Contents
unorderedSetInt =
  UnorderedSet.instantiate' "unorderedSetInt" intT intBoxReqs $
  UnorderedSet.defaultOptions { UnorderedSet.optValueConversion = Just ConvertValue }

unorderedSetIntBox :: UnorderedSet.Contents
unorderedSetIntBox =
  UnorderedSet.instantiate' "unorderedSetIntBox" (objT c_IntBoxEquatable) intBoxReqs $
  UnorderedSet.defaultOptions { UnorderedSet.optValueConversion = Just ConvertPtr }

vectorIntBox :: Vector.Contents
vectorIntBox =
  Vector.instantiate' "vectorIntBox" (objT c_IntBox) intBoxReqs $
  Vector.defaultOptions { Vector.optValueConversion = Just ConvertPtr }

vectorIntBoxComparable :: Vector.Contents
vectorIntBoxComparable = Vector.instantiate "vectorIntBoxComparable" (objT c_IntBox) intBoxReqs

vectorIntBoxEquatable :: Vector.Contents
vectorIntBoxEquatable = Vector.instantiate "vectorIntBoxEquatable" (objT c_IntBox) intBoxReqs

vectorString :: Vector.Contents
vectorString =
  Vector.instantiate' "vectorString" (objT c_string) intBoxReqs $
  Vector.defaultOptions { Vector.optValueConversion = Just ConvertValue }
