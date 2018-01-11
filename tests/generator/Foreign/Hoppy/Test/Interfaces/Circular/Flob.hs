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

module Foreign.Hoppy.Test.Interfaces.Circular.Flob (
  flobModule,
  c_FlobClass,
  ) where

import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Types
import Foreign.Hoppy.Test.Interfaces.Circular.Flub

flobModule :: Module
flobModule =
  addReqIncludes [includeLocal "flob.hpp"] $
  moduleModify' (makeModule "flob" "flobm.hpp" "flobm.cpp") $
  moduleAddExports
  [ ExportClass c_FlobClass
  , ExportFn f_takesFlubValues
  , ExportFn f_returnsFlubClass
  , ExportFn f_returnsFlubEnum
  , ExportFn f_returnsFlubBitspace
  ]

c_FlobClass :: Class
c_FlobClass =
  makeClass (ident "FlobClass") Nothing []
  [ mkCtor "new" []
  , mkConstMethod "invokeCallback" [callbackT cb_FlubCallback] voidT
  ]

f_takesFlubValues :: Function
f_takesFlubValues =
  makeFn (ident "takesFlubValues") Nothing Nonpure
  [ptrT $ objT c_FlubClass, enumT e_FlubEnum, bitspaceT bs_FlubBitspace] voidT

f_returnsFlubClass :: Function
f_returnsFlubClass =
  makeFn (ident "returnsFlubClass") Nothing Nonpure [] $ ptrT $ objT c_FlubClass

f_returnsFlubEnum :: Function
f_returnsFlubEnum =
  makeFn (ident "returnsFlubEnum") Nothing Nonpure [] $ enumT e_FlubEnum

f_returnsFlubBitspace :: Function
f_returnsFlubBitspace =
  makeFn (ident "returnsFlubBitspace") Nothing Nonpure [] $ bitspaceT bs_FlubBitspace
