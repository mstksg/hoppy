-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

module Foreign.Hoppy.Generator.Test.Circular.Flob (
  flobModule,
  c_FlobClass,
  ) where

import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Test.Circular.Flub

flobModule :: Module
flobModule =
  addReqIncludes [includeLocal "flob.hpp"] $
  modifyModule' (makeModule "flob" "flobm.hpp" "flobm.cpp") $
  addModuleExports
  [ ExportClass c_FlobClass
  , ExportFn f_takesFlubValues
  , ExportFn f_returnsFlubClass
  , ExportFn f_returnsFlubEnum
  , ExportFn f_returnsFlubBitspace
  ]

c_FlobClass :: Class
c_FlobClass =
  makeClass (ident "FlobClass") Nothing []
  [ mkCtor "new" [] ]
  [ mkConstMethod "invokeCallback" [TCallback cb_FlubCallback] TVoid ]

f_takesFlubValues :: Function
f_takesFlubValues =
  makeFn (ident "takesFlubValues") Nothing Nonpure
  [TPtr $ TObj c_FlubClass, TEnum e_FlubEnum, TBitspace bs_FlubBitspace] TVoid

f_returnsFlubClass :: Function
f_returnsFlubClass =
  makeFn (ident "returnsFlubClass") Nothing Nonpure [] $ TPtr $ TObj c_FlubClass

f_returnsFlubEnum :: Function
f_returnsFlubEnum =
  makeFn (ident "returnsFlubEnum") Nothing Nonpure [] $ TEnum e_FlubEnum

f_returnsFlubBitspace :: Function
f_returnsFlubBitspace =
  makeFn (ident "returnsFlubBitspace") Nothing Nonpure [] $ TBitspace bs_FlubBitspace
