module Foreign.Hoppy.Generator.Test.Circular.Flub (
  flubModule,
  e_FlubEnum,
  bs_FlubBitspace,
  c_FlubClass,
  cb_FlubCallback,
  ) where

import Foreign.Hoppy.Generator.Spec
import {-# SOURCE #-} Foreign.Hoppy.Generator.Test.Circular.Flob

flubModule :: Module
flubModule =
  addReqIncludes [includeLocal "flub.hpp"] $
  modifyModule' (makeModule "flub" "flubm.hpp" "flubm.cpp") $
  addModuleExports
  [ ExportVariable v_FlubVar
  , ExportVariable v_FlubVarConst
  , ExportEnum e_FlubEnum
  , ExportBitspace bs_FlubBitspace
  , ExportClass c_FlubClass
  , ExportFn f_takesFlobValues
  , ExportCallback cb_FlubCallback
  ]

v_FlubVar :: Variable
v_FlubVar = makeVariable (ident "flubVar") Nothing TChar

v_FlubVarConst :: Variable
v_FlubVarConst = makeVariable (ident "flubVarConst") Nothing $ TConst TChar

e_FlubEnum :: CppEnum
e_FlubEnum = makeEnum (ident "FlubEnum") Nothing enumValues

bs_FlubBitspace :: Bitspace
bs_FlubBitspace =
  bitspaceAddEnum e_FlubEnum $
  makeBitspace (toExtName "FlubBitspace") TInt enumValues

enumValues :: [(Int, [String])]
enumValues =
  [ (0x1, ["option", "a"])
  , (0x2, ["option", "b"])
  , (0x4, ["option", "c"])
  ]

c_FlubClass :: Class
c_FlubClass =
  makeClass (ident "FlubClass") Nothing []
  [ mkCtor "new" [] ]
  []

f_takesFlobValues :: Function
f_takesFlobValues =
  makeFn (ident "takesFlobValues") Nothing Nonpure [TPtr $ TObj c_FlobClass] TVoid

cb_FlubCallback :: Callback
cb_FlubCallback = makeCallback (toExtName "FlubCallback") [TPtr $ TObj c_FlubClass] TVoid
