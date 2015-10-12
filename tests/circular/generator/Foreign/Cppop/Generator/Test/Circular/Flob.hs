module Foreign.Cppop.Generator.Test.Circular.Flob (
  flobModule,
  c_FlobClass,
  ) where

import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Test.Circular.Flub

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
