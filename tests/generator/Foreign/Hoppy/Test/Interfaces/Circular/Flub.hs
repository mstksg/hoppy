-- This file is part of Hoppy.
--
-- Copyright 2015-2020 Bryan Gardiner <bog@khumba.net>
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
  c_FlubClass,
  cb_FlubCallback,
  ) where

import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Types
import {-# SOURCE #-} Foreign.Hoppy.Test.Interfaces.Circular.Flob

flubModule :: Module
flubModule =
  addReqIncludes [includeStd "flub.hpp"] $
  moduleModify' (makeModule "flub" "flubm.hpp" "flubm.cpp") $
  moduleAddExports
  [ toExport v_FlubVar
  , toExport v_FlubVarConst
  , toExport e_FlubEnum
  , toExport c_FlubClass
  , toExport f_takesFlobValues
  , toExport cb_FlubCallback
  ]

v_FlubVar :: Variable
v_FlubVar = makeVariable (ident "flubVar") Nothing charT

v_FlubVarConst :: Variable
v_FlubVarConst = makeVariable (ident "flubVarConst") Nothing $ constT charT

e_FlubEnum :: CppEnum
e_FlubEnum =
  addReqIncludes [includeStd "flub.hpp"] $
  makeEnum (ident "FlubEnum") Nothing
  [ (0x1, ["option", "a"])
  , (0x2, ["option", "b"])
  , (0x4, ["option", "c"])
  ]

c_FlubClass :: Class
c_FlubClass =
  makeClass (ident "FlubClass") Nothing []
  [ mkCtor "new" np
  , mkClassVariable "flubClassVar" intT
  , mkStaticClassVariable "flubStaticClassVar" intT
  , mkStaticClassVariable "flubStaticConstClassVar" $ constT intT
  ]

f_takesFlobValues :: Function
f_takesFlobValues =
  makeFn (ident "takesFlobValues") Nothing Nonpure [ptrT $ objT c_FlobClass] voidT

cb_FlubCallback :: Callback
cb_FlubCallback = makeCallback (toExtName "FlubCallback") [ptrT $ objT c_FlubClass] voidT
