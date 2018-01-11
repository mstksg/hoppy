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

module Foreign.Hoppy.Test.Interfaces.Basic (interfaceResult) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Foreign.Hoppy.Generator.Language.Haskell (addImports, sayLn)
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

interfaceResult :: Either String Interface
interfaceResult =
  interfaceAddHaskellModuleBase ["Foreign", "Hoppy", "Test"] =<<
  interfaceSetExceptionSupportModule testModule <$>
  interface "basic" modules

modules :: [Module]
modules = [testModule]

testModule :: Module
testModule =
  moduleModify' (makeModule "basic" "basic.hpp" "basic.cpp") $
  moduleAddExports
  [ -- Is this thing on?
    ExportClass c_IntBox
  , ExportClass c_PtrCtr
  , ExportClass c_PtrCtrWithToHeapConversion
  , ExportClass c_PtrCtrWithToGcConversion
  , ExportClass c_ClassWithAltPrefix
  , ExportClass c_ClassWithNoPrefix
  , ExportFn f_piapprox
  , ExportFn f_piapproxNonpure
  , ExportFn f_timesTwo
  , ExportFn f_takesLongFn
  , ExportCallback cb_LongCallback
  , ExportFn f_takesIntBoxFn
  , ExportCallback cb_IntBoxCallback
    -- For testing objToHeapT.
  , ExportFn f_givePtrCtrByValue
  , ExportFn f_givePtrCtrByValueToCallback
  , ExportCallback cb_GetPtrCtrByValueCallback
    -- Passing objects to C++.
  , ExportFn f_getBoxValueByValue
  , ExportFn f_getBoxValueByRef
  , ExportFn f_getBoxValueByRefConst
  , ExportFn f_getBoxValueByPtr
  , ExportFn f_getBoxValueByPtrConst
    -- Returning objects from C++.
  , ExportFn f_makeBoxByValue
  , ExportFn f_makeBoxByRef
  , ExportFn f_makeBoxByRefConst
  , ExportFn f_makeBoxByPtr
  , ExportFn f_makeBoxByPtrConst
    -- Passing objects to Haskell callbacks.
  , ExportCallback cb_GetBoxValueByValueCallback
  , ExportCallback cb_GetBoxValueByRefCallback
  , ExportCallback cb_GetBoxValueByRefConstCallback
  , ExportCallback cb_GetBoxValueByPtrCallback
  , ExportCallback cb_GetBoxValueByPtrConstCallback
    -- ...and the C++ drivers for the above.
  , ExportFn f_getBoxValueByValueCallbackDriver
  , ExportFn f_getBoxValueByRefCallbackDriver
  , ExportFn f_getBoxValueByRefConstCallbackDriver
  , ExportFn f_getBoxValueByPtrCallbackDriver
  , ExportFn f_getBoxValueByPtrConstCallbackDriver
    -- Returning objects from Haskell callbacks.
  , ExportCallback cb_MakeBoxByValueCallback
  , ExportCallback cb_MakeBoxByRefCallback
  , ExportCallback cb_MakeBoxByRefConstCallback
  , ExportCallback cb_MakeBoxByPtrCallback
  , ExportCallback cb_MakeBoxByPtrConstCallback
    -- ...and the C++ drivers for the above.
  , ExportFn f_makeBoxByValueCallbackDriver
  , ExportFn f_makeBoxByRefCallbackDriver
  , ExportFn f_makeBoxByRefConstCallbackDriver
  , ExportFn f_makeBoxByPtrCallbackDriver
  , ExportFn f_makeBoxByPtrConstCallbackDriver
    -- TToGc tests.
  , ExportFn f_makeBoxToGc
  , ExportCallback cb_takesBoxToGcCallback
  , ExportFn f_callBoxToGcCallback
    -- Testing FnMethod.
  , ExportClass c_IntBoxWithFnMethods
    -- Primitive type sizeof checks.
  , ExportFn f_isTrue
  , ExportFn f_isFalse
  , ExportFn f_sizeOfBool
  , ExportFn f_sizeOfChar
  , ExportFn f_sizeOfShort
  , ExportFn f_sizeOfInt
  , ExportFn f_sizeOfLong
  , ExportFn f_sizeOfLLong
  , ExportFn f_sizeOfFloat
  , ExportFn f_sizeOfDouble
  , ExportFn f_sizeOfPtrdiff
  , ExportFn f_sizeOfSize
  , ExportFn f_sizeOfSSize
    -- Numeric type passing tests.
  , ExportFn f_doubleInt
  , ExportFn f_doubleLong
  , ExportFn f_doubleFloat
  , ExportFn f_doubleDouble
  , ExportFn f_doubleInt8
  , ExportFn f_doubleInt32
  , ExportFn f_doubleUInt16
  , ExportFn f_doubleUInt64
    -- Testing raw pointers.
  , ExportFn f_getBoolPtr
  , ExportFn f_getIntPtr
  , ExportFn f_getIntPtrPtr
  , ExportFn f_getIntBoxPtrPtr
  , ExportFn f_doubleIntPtr
  , ExportFn f_doubleIntPtrPtr
  , ExportFn f_doubleIntRef
  , ExportFn f_doubleIntBoxPtrPtr
    -- Classes with private destructors.
  , ExportClass c_Undeletable
    -- Multiple inheritance tests.
  , ExportClass c_InheritanceA
  , ExportClass c_InheritanceB
  , ExportClass c_InheritanceC
    -- Enum and bitspace tests.
  , ExportEnum e_BetterBool
  , ExportBitspace bs_BetterBools
  , ExportFn f_betterBoolId
  , ExportFn f_betterBoolsId
  , ExportCallback cb_BetterBoolCallback
  , ExportCallback cb_BetterBoolsCallback
  , ExportFn f_takesBetterBoolCallback
  , ExportFn f_takesBetterBoolsCallback
    -- Exception tests.
  , ExportClass c_BaseException
  , ExportClass c_FileException
  , ExportClass c_ReadException
  , ExportClass c_WriteException
  , ExportFn f_throwsBaseException
  , ExportFn f_throwsFileException
  , ExportFn f_throwsReadException
  , ExportFn f_throwsWriteException
  , ExportFn f_throwsPtrCtr
  , ExportFn f_throwsAny
  , ExportCallback cb_ThrowingCallback
  , ExportFn f_invokeThrowingCallback
  , ExportFn f_throwingReturnBool
  , ExportFn f_throwingReturnInt
  , ExportFn f_throwingReturnIntBox
  , ExportCallback cb_ThrowingMakeBoxByValueCallback
  , ExportFn f_throwingMakeBoxByValueCallbackDriver
  ]

c_IntBox :: Class
c_IntBox =
  addReqIncludes [includeLocal "intbox.hpp"] $
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = Just $ do
      addImports $ hsWholeModuleImport "Foreign.Hoppy.Test.Basic.HsBox"
      return $ HsTyCon $ UnQual $ HsIdent "HsBox"
    , classHaskellConversionToCppFn = Just $ sayLn "intBox_newWithValue . getHsBox"
    , classHaskellConversionFromCppFn = Just $ do
      addImports $ hsImports "Prelude" ["(.)", "fmap"]
      sayLn "fmap HsBox . intBox_get"
    } $
  makeClass (ident "IntBox") Nothing []
  [ mkCtor "new" []
  , mkCtor "newWithValue" [intT]
  , mkConstMethod "get" [] intT
  , mkMethod "set" [intT] voidT
  ]

c_PtrCtr :: Class
c_PtrCtr =
  addReqIncludes [includeLocal "ptrctr.hpp"] $
  -- This class is an exception, so that we can test the lifecycle of exception
  -- objects.
  classMakeException $
  classAddFeatures [Copyable] $
  makeClass (ident "PtrCtr") Nothing []
  [ mkCtor "new" []
  , mkStaticMethod "newGcedObj" [] $ toGcT $ objT c_PtrCtr
  , mkStaticMethod "newGcedRefConst" [] $ toGcT $ refT $ constT $ objT c_PtrCtr
  , mkStaticMethod "newGcedRef" [] $ toGcT $ refT $ objT c_PtrCtr
  , mkStaticMethod "newGcedPtrConst" [] $ toGcT $ ptrT $ constT $ objT c_PtrCtr
  , mkStaticMethod "newGcedPtr" [] $ toGcT $ ptrT $ objT c_PtrCtr
  , mkStaticMethod "resetCounters" [] voidT
  , mkStaticMethod "getConstructionCount" [] intT
  , mkStaticMethod "getDestructionCount" [] intT
  , mkConstMethod "redButton" [] voidT
  ]

c_PtrCtrWithToHeapConversion :: Class
c_PtrCtrWithToHeapConversion =
  addReqIncludes [includeLocal "ptrctr.hpp"] $
  classSetConversionToHeap $
  classAddFeatures [Copyable] $
  makeClass (ident "PtrCtr") (Just $ toExtName "PtrCtrWithToHeapConversion") []
  [ mkCtor "new" []
  , mkStaticMethod' "newGcedObj" "newHeapObj" [] $ objT c_PtrCtrWithToHeapConversion
  , mkStaticMethod "resetCounters" [] voidT
  , mkStaticMethod "getConstructionCount" [] intT
  , mkStaticMethod "getDestructionCount" [] intT
  ]

c_PtrCtrWithToGcConversion :: Class
c_PtrCtrWithToGcConversion =
  addReqIncludes [includeLocal "ptrctr.hpp"] $
  classSetConversionToGc $
  classAddFeatures [Copyable] $
  makeClass (ident "PtrCtr") (Just $ toExtName "PtrCtrWithToGcConversion") []
  [ mkCtor "new" []
  , mkStaticMethod "newGcedObj" [] $ objT c_PtrCtrWithToGcConversion
  , mkStaticMethod "resetCounters" [] voidT
  , mkStaticMethod "getConstructionCount" [] intT
  , mkStaticMethod "getDestructionCount" [] intT
  ]

c_ClassWithAltPrefix :: Class
c_ClassWithAltPrefix =
  addReqIncludes [includeLocal "class-prefixes.hpp"] $
  classSetEntityPrefix "AltPrefixClass_" $
  makeClass (ident "ClassWithAltPrefix") Nothing []
  [ mkStaticMethod "foo" [] intT ]

c_ClassWithNoPrefix :: Class
c_ClassWithNoPrefix =
  addReqIncludes [includeLocal "class-prefixes.hpp"] $
  classSetEntityPrefix "" $
  makeClass (ident "ClassWithNoPrefix") Nothing []
  [ mkCtor "ctorWithNoPrefix" []
  , mkMethod "methodWithNoPrefix" [] intT
  ]

f_piapprox :: Function
f_piapprox =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "piapprox") Nothing Pure [] intT

f_piapproxNonpure :: Function
f_piapproxNonpure =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "piapprox") (Just $ toExtName "piapproxNonpure") Nonpure [] intT

f_timesTwo :: Function
f_timesTwo =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "timesTwo") Nothing Pure [longT] longT

f_takesLongFn :: Function
f_takesLongFn =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "takesLongFn") Nothing Pure [ptrT $ fnT [longT] longT, longT] longT

cb_LongCallback :: Callback
cb_LongCallback =
  makeCallback (toExtName "LongCallback") [longT] longT

f_takesIntBoxFn :: Function
f_takesIntBoxFn =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "takesIntBoxFn") Nothing Nonpure
  [ptrT $ fnT [ptrT $ objT c_IntBox] $ ptrT $ objT c_IntBox, intT] intT

cb_IntBoxCallback :: Callback
cb_IntBoxCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "IntBoxCallback") [ptrT $ objT c_IntBox] $ ptrT $ objT c_IntBox

f_givePtrCtrByValue :: Function
f_givePtrCtrByValue =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "givePtrCtrByValue") Nothing Nonpure [] $ objToHeapT c_PtrCtr

f_givePtrCtrByValueToCallback :: Function
f_givePtrCtrByValueToCallback =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "givePtrCtrByValueToCallback") Nothing Nonpure
  [callbackT cb_GetPtrCtrByValueCallback] voidT

cb_GetPtrCtrByValueCallback :: Callback
cb_GetPtrCtrByValueCallback =
  addReqIncludes [includeLocal "ptrctr.hpp"] $
  makeCallback (toExtName "GetPtrCtrByValueCallback") [objToHeapT c_PtrCtr] voidT

f_getBoxValueByValue :: Function
f_getBoxValueByValue =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByValue") Nothing Nonpure [objT c_IntBox] intT

f_getBoxValueByRef :: Function
f_getBoxValueByRef =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByRef") Nothing Nonpure [refT $ objT c_IntBox] intT

f_getBoxValueByRefConst :: Function
f_getBoxValueByRefConst =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByRefConst") Nothing Nonpure [refT $ constT $ objT c_IntBox] intT

f_getBoxValueByPtr :: Function
f_getBoxValueByPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByPtr") Nothing Nonpure [ptrT $ objT c_IntBox] intT

f_getBoxValueByPtrConst :: Function
f_getBoxValueByPtrConst =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByPtrConst") Nothing Nonpure [ptrT $ constT $ objT c_IntBox] intT

f_makeBoxByValue :: Function
f_makeBoxByValue =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByValue") Nothing Nonpure [intT] $ objT c_IntBox

f_makeBoxByRef :: Function
f_makeBoxByRef =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByRef") Nothing Nonpure [intT] $ refT $ objT c_IntBox

f_makeBoxByRefConst :: Function
f_makeBoxByRefConst =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByRefConst") Nothing Nonpure [intT] $ refT $ constT $ objT c_IntBox

f_makeBoxByPtr :: Function
f_makeBoxByPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByPtr") Nothing Nonpure [intT] $ ptrT $ objT c_IntBox

f_makeBoxByPtrConst :: Function
f_makeBoxByPtrConst =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByPtrConst") Nothing Nonpure [intT] $ ptrT $ constT $ objT c_IntBox

cb_GetBoxValueByValueCallback :: Callback
cb_GetBoxValueByValueCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByValueCallback") [objT c_IntBox] intT

cb_GetBoxValueByRefCallback :: Callback
cb_GetBoxValueByRefCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByRefCallback") [refT $ objT c_IntBox] intT

cb_GetBoxValueByRefConstCallback :: Callback
cb_GetBoxValueByRefConstCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByRefConstCallback") [refT $ constT $ objT c_IntBox] intT

cb_GetBoxValueByPtrCallback :: Callback
cb_GetBoxValueByPtrCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByPtrCallback") [ptrT $ objT c_IntBox] intT

cb_GetBoxValueByPtrConstCallback :: Callback
cb_GetBoxValueByPtrConstCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByPtrConstCallback") [ptrT $ constT $ objT c_IntBox] intT

f_getBoxValueByValueCallbackDriver :: Function
f_getBoxValueByValueCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByValueCallbackDriver") Nothing Nonpure
  [callbackT cb_GetBoxValueByValueCallback, intT] intT

f_getBoxValueByRefCallbackDriver :: Function
f_getBoxValueByRefCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByRefCallbackDriver") Nothing Nonpure
  [callbackT cb_GetBoxValueByRefCallback, intT] intT

f_getBoxValueByRefConstCallbackDriver :: Function
f_getBoxValueByRefConstCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByRefConstCallbackDriver") Nothing Nonpure
  [callbackT cb_GetBoxValueByRefConstCallback, intT] intT

f_getBoxValueByPtrCallbackDriver :: Function
f_getBoxValueByPtrCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByPtrCallbackDriver") Nothing Nonpure
  [callbackT cb_GetBoxValueByPtrCallback, intT] intT

f_getBoxValueByPtrConstCallbackDriver :: Function
f_getBoxValueByPtrConstCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByPtrConstCallbackDriver") Nothing Nonpure
  [callbackT cb_GetBoxValueByPtrConstCallback, intT] intT

cb_MakeBoxByValueCallback :: Callback
cb_MakeBoxByValueCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByValueCallback") [intT] $ objT c_IntBox

cb_MakeBoxByRefCallback :: Callback
cb_MakeBoxByRefCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByRefCallback") [intT] $ refT $ objT c_IntBox

cb_MakeBoxByRefConstCallback :: Callback
cb_MakeBoxByRefConstCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByRefConstCallback") [intT] $ refT $ constT $ objT c_IntBox

cb_MakeBoxByPtrCallback :: Callback
cb_MakeBoxByPtrCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByPtrCallback") [intT] $ ptrT $ objT c_IntBox

cb_MakeBoxByPtrConstCallback :: Callback
cb_MakeBoxByPtrConstCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByPtrConstCallback") [intT] $ ptrT $ constT $ objT c_IntBox

f_makeBoxByValueCallbackDriver :: Function
f_makeBoxByValueCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByValueCallbackDriver") Nothing Nonpure
  [callbackT cb_MakeBoxByValueCallback, intT] intT

f_makeBoxByRefCallbackDriver :: Function
f_makeBoxByRefCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByRefCallbackDriver") Nothing Nonpure
  [callbackT cb_MakeBoxByRefCallback, intT] intT

f_makeBoxByRefConstCallbackDriver :: Function
f_makeBoxByRefConstCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByRefConstCallbackDriver") Nothing Nonpure
  [callbackT cb_MakeBoxByRefConstCallback, intT] intT

f_makeBoxByPtrCallbackDriver :: Function
f_makeBoxByPtrCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByPtrCallbackDriver") Nothing Nonpure
  [callbackT cb_MakeBoxByPtrCallback, intT] intT

f_makeBoxByPtrConstCallbackDriver :: Function
f_makeBoxByPtrConstCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByPtrConstCallbackDriver") Nothing Nonpure
  [callbackT cb_MakeBoxByPtrConstCallback, intT] intT

f_makeBoxToGc :: Function
f_makeBoxToGc =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxToGc") Nothing Nonpure [intT] $ toGcT $ objT c_IntBox

cb_takesBoxToGcCallback :: Callback
cb_takesBoxToGcCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "TakesBoxToGcCallback") [toGcT $ objT c_IntBox] $ intT

f_callBoxToGcCallback :: Function
f_callBoxToGcCallback =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "callBoxToGcCallback") Nothing Nonpure
  [callbackT cb_takesBoxToGcCallback, intT] intT

c_IntBoxWithFnMethods :: Class
c_IntBoxWithFnMethods =
  addReqIncludes [includeLocal "functions.hpp", includeLocal "intbox.hpp"] $
  makeClass (ident "IntBox") (Just $ toExtName "IntBoxWithFnMethods") []
  [ mkCtor "new" [intT]
  , -- A normal method.
    makeFnMethod (ident "getBoxValueByRef") "getValue" MNormal Nonpure
    [refT $ objT c_IntBoxWithFnMethods] intT
    -- A const method.
  , makeFnMethod (ident "getBoxValueByPtrConst") "getValueConst" MConst Nonpure
    [ptrT $ constT $ objT c_IntBoxWithFnMethods] intT
    -- A static method.
  , makeFnMethod (ident "timesTwo") "double" MStatic Pure [longT] longT
  ]

f_isTrue :: Function
f_isTrue =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "isTrue") Nothing Nonpure [boolT] boolT

f_isFalse :: Function
f_isFalse =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "isFalse") Nothing Nonpure [boolT] boolT

f_sizeOfBool :: Function
f_sizeOfBool =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfBool") Nothing Pure [] sizeT

f_sizeOfChar :: Function
f_sizeOfChar =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfChar") Nothing Pure [] sizeT

f_sizeOfShort :: Function
f_sizeOfShort =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfShort") Nothing Pure [] sizeT

f_sizeOfInt :: Function
f_sizeOfInt =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfInt") Nothing Pure [] sizeT

f_sizeOfLong :: Function
f_sizeOfLong =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfLong") Nothing Pure [] sizeT

f_sizeOfLLong :: Function
f_sizeOfLLong =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfLLong") Nothing Pure [] sizeT

f_sizeOfFloat :: Function
f_sizeOfFloat =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfFloat") Nothing Pure [] sizeT

f_sizeOfDouble :: Function
f_sizeOfDouble =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfDouble") Nothing Pure [] sizeT

f_sizeOfPtrdiff :: Function
f_sizeOfPtrdiff =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfPtrdiff") Nothing Pure [] sizeT

f_sizeOfSize :: Function
f_sizeOfSize =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfSize") Nothing Pure [] sizeT

f_sizeOfSSize :: Function
f_sizeOfSSize =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfSSize") Nothing Pure [] sizeT

f_doubleInt :: Function
f_doubleInt =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleInt") Nothing Pure [intT] intT

f_doubleLong :: Function
f_doubleLong =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleLong") Nothing Pure [longT] longT

f_doubleFloat :: Function
f_doubleFloat =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleFloat") Nothing Pure [floatT] floatT

f_doubleDouble :: Function
f_doubleDouble =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleDouble") Nothing Pure [doubleT] doubleT

f_doubleInt8 :: Function
f_doubleInt8 =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleInt8") Nothing Pure [int8T] int8T

f_doubleInt32 :: Function
f_doubleInt32 =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleInt32") Nothing Pure [int32T] int32T

f_doubleUInt16 :: Function
f_doubleUInt16 =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleUInt16") Nothing Pure [word16T] word16T

f_doubleUInt64 :: Function
f_doubleUInt64 =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleUInt64") Nothing Pure [word64T] word64T

f_getBoolPtr :: Function
f_getBoolPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoolPtr") Nothing Nonpure [] $ ptrT boolT

f_getIntPtr :: Function
f_getIntPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getIntPtr") Nothing Nonpure [] $ ptrT intT

f_getIntPtrPtr :: Function
f_getIntPtrPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getIntPtrPtr") Nothing Nonpure [] $ ptrT $ ptrT intT

f_getIntBoxPtrPtr :: Function
f_getIntBoxPtrPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getIntBoxPtrPtr") Nothing Nonpure [] $ ptrT $ ptrT $ objT c_IntBox

f_doubleIntPtr :: Function
f_doubleIntPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleIntPtr") Nothing Nonpure [ptrT intT] voidT

f_doubleIntPtrPtr :: Function
f_doubleIntPtrPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleIntPtrPtr") Nothing Nonpure [ptrT $ ptrT intT] voidT

f_doubleIntRef :: Function
f_doubleIntRef =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleIntRef") Nothing Nonpure [refT intT] voidT

f_doubleIntBoxPtrPtr :: Function
f_doubleIntBoxPtrPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleIntBoxPtrPtr") Nothing Nonpure [ptrT $ ptrT $ objT c_IntBox] voidT

c_Undeletable :: Class
c_Undeletable =
  addReqIncludes [includeLocal "undeletable.hpp"] $
  classSetDtorPrivate $
  makeClass (ident "Undeletable") Nothing []
  [ mkStaticMethod "getInstance" [] $ refT $ objT c_Undeletable
  ]

c_InheritanceA :: Class
c_InheritanceA =
  addReqIncludes [includeLocal "inheritance.hpp"] $
  makeClass (ident "InheritanceA") Nothing []
  [ mkCtor "new" []
  , mkConstMethod "aFoo" [] intT
  , mkConstMethod "aBar" [] intT
  ]

c_InheritanceB :: Class
c_InheritanceB =
  addReqIncludes [includeLocal "inheritance.hpp"] $
  makeClass (ident "InheritanceB") Nothing []
  [ mkConstMethod "bFoo" [] intT
  ]

c_InheritanceC :: Class
c_InheritanceC =
  addReqIncludes [includeLocal "inheritance.hpp"] $
  makeClass (ident "InheritanceC") Nothing [c_InheritanceA, c_InheritanceB]
  [ mkCtor "new" []
  ]

(e_BetterBool, bs_BetterBools) =
  let enum = makeEnum (ident "BetterBool") Nothing values
  in (enum,
      bitspaceAddEnum enum $
      bitspaceAddCppType (ident "BetterBool") (Just "static_cast<BetterBool>") Nothing $
      makeBitspace (toExtName "BetterBools") intT values)
  where values =
          [ (0, ["true"])
          , (1, ["false"])
          , (4, ["file", "not", "found"])
          ]

f_betterBoolId :: Function
f_betterBoolId =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "betterBoolId") Nothing Pure [enumT e_BetterBool] $ enumT e_BetterBool

f_betterBoolsId :: Function
f_betterBoolsId =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "betterBoolId") (Just $ toExtName "betterBoolsId") Pure
  [bitspaceT bs_BetterBools] $ bitspaceT bs_BetterBools

cb_BetterBoolCallback :: Callback
cb_BetterBoolCallback =
  addReqIncludes [includeLocal "enum.hpp"] $
  makeCallback (toExtName "BetterBoolCallback") [enumT e_BetterBool] $ enumT e_BetterBool

cb_BetterBoolsCallback :: Callback
cb_BetterBoolsCallback =
  addReqIncludes [includeLocal "enum.hpp"] $
  makeCallback (toExtName "BetterBoolsCallback")
  [bitspaceT bs_BetterBools] $ bitspaceT bs_BetterBools

f_takesBetterBoolCallback :: Function
f_takesBetterBoolCallback =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "takesBetterBoolCallback") Nothing Pure
  [callbackT cb_BetterBoolCallback, enumT e_BetterBool] $ enumT e_BetterBool

f_takesBetterBoolsCallback :: Function
f_takesBetterBoolsCallback =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "takesBetterBoolsCallback") Nothing Pure
  [callbackT cb_BetterBoolsCallback, bitspaceT bs_BetterBools] $ bitspaceT bs_BetterBools

c_BaseException :: Class
c_BaseException =
  addReqIncludes [includeLocal "exceptions.hpp"] $
  classMakeException $
  classAddFeatures [Copyable] $
  makeClass (ident "BaseException") Nothing []
  [ mkCtor "new" [] ]

c_FileException :: Class
c_FileException =
  addReqIncludes [includeLocal "exceptions.hpp"] $
  classMakeException $
  classAddFeatures [Copyable] $
  makeClass (ident "FileException") Nothing [c_BaseException]
  [ mkCtor "new" [] ]

c_ReadException :: Class
c_ReadException =
  addReqIncludes [includeLocal "exceptions.hpp"] $
  classMakeException $
  classAddFeatures [Copyable] $
  makeClass (ident "ReadException") Nothing [c_FileException]
  [ mkCtor "new" [] ]

c_WriteException :: Class
c_WriteException =
  addReqIncludes [includeLocal "exceptions.hpp"] $
  classMakeException $
  classAddFeatures [Copyable] $
  makeClass (ident "WriteException") Nothing [c_FileException]
  [ mkCtor "new" [] ]

f_throwsBaseException :: Function
f_throwsBaseException =
  addReqIncludes [includeLocal "functions.hpp"] $
  handleExceptions [CatchClass c_BaseException] $
  makeFn (ident "throwsBaseException") Nothing Nonpure [] voidT

f_throwsFileException :: Function
f_throwsFileException =
  addReqIncludes [includeLocal "functions.hpp"] $
  handleExceptions [CatchClass c_FileException] $
  makeFn (ident "throwsFileException") Nothing Nonpure [] voidT

f_throwsReadException :: Function
f_throwsReadException =
  addReqIncludes [includeLocal "functions.hpp"] $
  handleExceptions [CatchClass c_ReadException] $
  makeFn (ident "throwsReadException") Nothing Nonpure [] voidT

f_throwsWriteException :: Function
f_throwsWriteException =
  addReqIncludes [includeLocal "functions.hpp"] $
  handleExceptions [CatchClass c_WriteException] $
  makeFn (ident "throwsWriteException") Nothing Nonpure [] voidT

f_throwsPtrCtr :: Function
f_throwsPtrCtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  handleExceptions [CatchClass c_PtrCtr] $
  makeFn (ident "throwsPtrCtr") Nothing Nonpure [] voidT

f_throwsAny :: Function
f_throwsAny =
  addReqIncludes [includeLocal "functions.hpp"] $
  handleExceptions [CatchAll] $
  makeFn (ident "throwsAny") Nothing Nonpure [intT] voidT

cb_ThrowingCallback :: Callback
cb_ThrowingCallback =
  callbackSetThrows True $
  makeCallback (toExtName "ThrowingCallback") [] voidT

f_invokeThrowingCallback :: Function
f_invokeThrowingCallback =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "invokeThrowingCallback") Nothing Nonpure
  [callbackT cb_ThrowingCallback] intT

-- This ensures that generated exception handling code plays well with the
-- conversions necessary for returning booleans from a function.
f_throwingReturnBool :: Function
f_throwingReturnBool =
  addReqIncludes [includeLocal "functions.hpp"] $
  handleExceptions [CatchClass c_BaseException] $
  makeFn (ident "throwingReturnBool") Nothing Nonpure [] boolT

-- This ensures that generated exception handling code plays well with generated
-- code that returns ints from functions -- ints require no conversions, unlike
-- other types.
f_throwingReturnInt :: Function
f_throwingReturnInt =
  addReqIncludes [includeLocal "functions.hpp"] $
  handleExceptions [CatchClass c_BaseException] $
  makeFn (ident "throwingReturnInt") Nothing Nonpure [] intT

-- This ensures that generated exception handling code plays well with the
-- conversions necessary for returning objects from a function.
f_throwingReturnIntBox :: Function
f_throwingReturnIntBox =
  addReqIncludes [includeLocal "functions.hpp"] $
  handleExceptions [CatchClass c_BaseException] $
  makeFn (ident "throwingReturnIntBox") Nothing Nonpure [] $ objT c_IntBox

-- This ensures that generated exception handling code plays well with the
-- conversions necessary for returning objects from a callback.
cb_ThrowingMakeBoxByValueCallback :: Callback
cb_ThrowingMakeBoxByValueCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  callbackSetThrows True $
  makeCallback (toExtName "ThrowingMakeBoxByValueCallback") [intT] $ objT c_IntBox

f_throwingMakeBoxByValueCallbackDriver :: Function
f_throwingMakeBoxByValueCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  handleExceptions [CatchClass c_BaseException] $
  makeFn (ident "throwingMakeBoxByValueCallbackDriver") Nothing Nonpure
  [callbackT cb_ThrowingMakeBoxByValueCallback, intT] intT
