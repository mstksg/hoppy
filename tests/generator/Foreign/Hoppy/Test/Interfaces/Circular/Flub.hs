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

module Foreign.Hoppy.Test.Interfaces.Circular.Flub (
  flubModule,
  e_FlubEnum,
  bs_FlubBitspace,
  c_FlubClass,
  cb_FlubCallback,
  ) where

import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Types
import {-# SOURCE #-} Foreign.Hoppy.Test.Interfaces.Circular.Flob

flubModule :: Module
flubModule =
  addReqIncludes [includeLocal "flub.hpp"] $
  moduleModify' (makeModule "flub" "flubm.hpp" "flubm.cpp") $
  moduleAddExports
  [ ExportVariable v_FlubVar
  , ExportVariable v_FlubVarConst
  , ExportEnum e_FlubEnum
  , ExportBitspace bs_FlubBitspace
  , ExportClass c_FlubClass
  , ExportFn f_takesFlobValues
  , ExportCallback cb_FlubCallback
  ]

v_FlubVar :: Variable
v_FlubVar = makeVariable (ident "flubVar") Nothing charT

v_FlubVarConst :: Variable
v_FlubVarConst = makeVariable (ident "flubVarConst") Nothing $ constT charT

e_FlubEnum :: CppEnum
e_FlubEnum = makeEnum (ident "FlubEnum") Nothing enumValues

bs_FlubBitspace :: Bitspace
bs_FlubBitspace =
  bitspaceAddEnum e_FlubEnum $
  makeBitspace (toExtName "FlubBitspace") intT enumValues

enumValues :: [(Int, [String])]
enumValues =
  [ (0x1, ["option", "a"])
  , (0x2, ["option", "b"])
  , (0x4, ["option", "c"])
  ]

c_FlubClass :: Class
c_FlubClass =
  makeClass (ident "FlubClass") Nothing []
  [ mkCtor "new" []
  , mkClassVariable "flubClassVar" intT
  , mkStaticClassVariable "flubStaticClassVar" intT
  , mkStaticClassVariable "flubStaticConstClassVar" $ constT intT
  ]

f_takesFlobValues :: Function
f_takesFlobValues =
  makeFn (ident "takesFlobValues") Nothing Nonpure [ptrT $ objT c_FlobClass] voidT

cb_FlubCallback :: Callback
cb_FlubCallback = makeCallback (toExtName "FlubCallback") [ptrT $ objT c_FlubClass] voidT
