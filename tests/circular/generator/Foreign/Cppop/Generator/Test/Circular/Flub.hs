module Foreign.Cppop.Generator.Test.Circular.Flub (
  flubModule,
  e_FlubEnum,
  bs_FlubBitspace,
  c_FlubClass,
  cb_FlubCallback,
  ) where

import Foreign.Cppop.Generator.Spec
import {-# SOURCE #-} Foreign.Cppop.Generator.Test.Circular.Flob

flubModule :: Module
flubModule =
  addReqIncludes [includeLocal "flub.hpp"] $
  modifyModule' (makeModule "flub" "flubm.hpp" "flubm.cpp") $
  addModuleExports
  [ ExportEnum e_FlubEnum
  , ExportBitspace bs_FlubBitspace
  , ExportClass c_FlubClass
  , ExportFn f_takesFlobValues
  , ExportCallback cb_FlubCallback
  ]

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
