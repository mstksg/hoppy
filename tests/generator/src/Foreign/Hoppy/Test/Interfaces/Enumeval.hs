-- This file is part of Hoppy.
--
-- Copyright 2015-2023 Bryan Gardiner <bog@khumba.net>
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

module Foreign.Hoppy.Test.Interfaces.Enumeval (interfaceResult) where

import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Types (intT)

{-# ANN module "HLint: ignore Use camelCase" #-}

interfaceResult :: Either String Interface
interfaceResult =
  interface "enumeval" modules >>=
  interfaceAddHaskellModuleBase ["Foreign", "Hoppy", "Test"]

modules :: [Module]
modules = [testModule]

testModule :: Module
testModule =
  moduleModify' (makeModule "enumeval" "enumeval.hpp" "enumeval.cpp") $
  moduleAddExports
  [ toExport e_IceCream
  , toExport e_Number
  , toExport f_rankIceCream
  ]

enumsInclude :: Include
enumsInclude = includeStd "enums.hpp"

e_IceCream :: CppEnum
e_IceCream =
  addReqIncludes [enumsInclude] $
  makeAutoEnum (ident "IceCream") Nothing Unscoped
  [ "CHOCOLATE"
  , "BUBBLEGUM"
  , "BIRTHDAY_CAKE"
  ]

e_Number :: CppEnum
e_Number =
  addReqIncludes [enumsInclude] $
  makeAutoEnum (ident "Number") Nothing Scoped
  [ "one"
  , "oneAndAHalf"
  , "Two"
  , "THREE"
  , "four_fiveSixSEVEN"
  ]

f_rankIceCream :: Function
f_rankIceCream =
  addReqIncludes [enumsInclude] $
  makeFn (ident "rankIceCream") Nothing Pure [enumT e_IceCream] intT
