-- This file is part of Hoppy.
--
-- Copyright 2015-2019 Bryan Gardiner <bog@khumba.net>
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
import Foreign.Hoppy.Test.Interfaces.Compiler (makeTestCompiler)

{-# ANN module "HLint: ignore Use camelCase" #-}

interfaceResult :: Either String Interface
interfaceResult =
  interface "basic" modules >>=
  pure . interfaceSetCompiler (makeTestCompiler "basic") >>=
  pure . interfaceSetExceptionSupportModule testModule >>=
  interfaceAddHaskellModuleBase ["Foreign", "Hoppy", "Test"]

modules :: [Module]
modules = [testModule]

testModule :: Module
testModule =
  moduleModify' (makeModule "basic" "basic.hpp" "basic.cpp") $
  moduleAddExports
  [ -- Is this thing on?
    toExport c_IntBox
  , toExport c_PtrCtr
  , toExport c_PtrCtrWithToHeapConversion
  , toExport c_PtrCtrWithToGcConversion
  , toExport c_ClassWithAltPrefix
  , toExport c_ClassWithNoPrefix
  , toExport f_piapprox
  , toExport f_piapproxNonpure
  , toExport f_timesTwo
  , toExport f_takesLongFn
  , toExport cb_LongCallback
  , toExport f_takesIntBoxFn
  , toExport cb_IntBoxCallback
    -- For testing objToHeapT.
  , toExport f_givePtrCtrByValue
  , toExport f_givePtrCtrByValueToCallback
  , toExport cb_GetPtrCtrByValueCallback
    -- Passing objects to C++.
  , toExport f_getBoxValueByValue
  , toExport f_getBoxValueByRef
  , toExport f_getBoxValueByRefConst
  , toExport f_getBoxValueByPtr
  , toExport f_getBoxValueByPtrConst
    -- Returning objects from C++.
  , toExport f_makeBoxByValue
  , toExport f_makeBoxByRef
  , toExport f_makeBoxByRefConst
  , toExport f_makeBoxByPtr
  , toExport f_makeBoxByPtrConst
    -- Passing objects to Haskell callbacks.
  , toExport cb_GetBoxValueByValueCallback
  , toExport cb_GetBoxValueByRefCallback
  , toExport cb_GetBoxValueByRefConstCallback
  , toExport cb_GetBoxValueByPtrCallback
  , toExport cb_GetBoxValueByPtrConstCallback
    -- ...and the C++ drivers for the above.
  , toExport f_getBoxValueByValueCallbackDriver
  , toExport f_getBoxValueByRefCallbackDriver
  , toExport f_getBoxValueByRefConstCallbackDriver
  , toExport f_getBoxValueByPtrCallbackDriver
  , toExport f_getBoxValueByPtrConstCallbackDriver
    -- Returning objects from Haskell callbacks.
  , toExport cb_MakeBoxByValueCallback
  , toExport cb_MakeBoxByRefCallback
  , toExport cb_MakeBoxByRefConstCallback
  , toExport cb_MakeBoxByPtrCallback
  , toExport cb_MakeBoxByPtrConstCallback
    -- ...and the C++ drivers for the above.
  , toExport f_makeBoxByValueCallbackDriver
  , toExport f_makeBoxByRefCallbackDriver
  , toExport f_makeBoxByRefConstCallbackDriver
  , toExport f_makeBoxByPtrCallbackDriver
  , toExport f_makeBoxByPtrConstCallbackDriver
    -- TToGc tests.
  , toExport f_makeBoxToGc
  , toExport cb_takesBoxToGcCallback
  , toExport f_callBoxToGcCallback
    -- Testing FnMethod.
  , toExport c_IntBoxWithFnMethods
    -- Primitive type sizeof checks.
  , toExport f_isTrue
  , toExport f_isFalse
  , toExport f_sizeOfBool
  , toExport f_sizeOfChar
  , toExport f_sizeOfShort
  , toExport f_sizeOfInt
  , toExport f_sizeOfLong
  , toExport f_sizeOfLLong
  , toExport f_sizeOfFloat
  , toExport f_sizeOfDouble
  , toExport f_sizeOfPtrdiff
  , toExport f_sizeOfSize
  , toExport f_sizeOfSSize
    -- Numeric type passing tests.
  , toExport f_doubleInt
  , toExport f_doubleLong
  , toExport f_doubleFloat
  , toExport f_doubleDouble
  , toExport f_doubleInt8
  , toExport f_doubleInt32
  , toExport f_doubleUInt16
  , toExport f_doubleUInt64
    -- Testing raw pointers.
  , toExport f_getBoolPtr
  , toExport f_getIntPtr
  , toExport f_getIntPtrPtr
  , toExport f_getIntBoxPtrPtr
  , toExport f_doubleIntPtr
  , toExport f_doubleIntPtrPtr
  , toExport f_doubleIntRef
  , toExport f_doubleIntBoxPtrPtr
    -- Classes with private destructors.
  , toExport c_Undeletable
    -- Multiple inheritance tests.
  , toExport c_InheritanceA
  , toExport c_InheritanceB
  , toExport c_InheritanceC
    -- Enum tests.
  , toExport e_BetterBool
  , toExport f_betterBoolId
  , toExport cb_BetterBoolCallback
  , toExport f_takesBetterBoolCallback
    -- Exception tests.
  , toExport c_BaseException
  , toExport c_FileException
  , toExport c_ReadException
  , toExport c_WriteException
  , toExport f_throwsBaseException
  , toExport f_throwsFileException
  , toExport f_throwsReadException
  , toExport f_throwsWriteException
  , toExport f_throwsPtrCtr
  , toExport f_throwsAny
  , toExport cb_ThrowingCallback
  , toExport f_invokeThrowingCallback
  , toExport f_throwingReturnBool
  , toExport f_throwingReturnInt
  , toExport f_throwingReturnIntBox
  , toExport cb_ThrowingMakeBoxByValueCallback
  , toExport f_throwingMakeBoxByValueCallbackDriver
  ]

c_IntBox :: Class
c_IntBox =
  addReqIncludes [includeStd "intbox.hpp"] $
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
  [ mkCtor "new" np
  , mkCtor "newWithValue" [intT]
  , mkConstMethod "get" np intT
  , mkMethod "set" [intT] voidT
  ]

c_PtrCtr :: Class
c_PtrCtr =
  addReqIncludes [includeStd "ptrctr.hpp"] $
  -- This class is an exception, so that we can test the lifecycle of exception
  -- objects.
  classMakeException $
  classAddFeatures [Copyable] $
  makeClass (ident "PtrCtr") Nothing []
  [ mkCtor "new" np
  , mkStaticMethod "newGcedObj" np $ toGcT $ objT c_PtrCtr
  , mkStaticMethod "newGcedRefConst" np $ toGcT $ refT $ constT $ objT c_PtrCtr
  , mkStaticMethod "newGcedRef" np $ toGcT $ refT $ objT c_PtrCtr
  , mkStaticMethod "newGcedPtrConst" np $ toGcT $ ptrT $ constT $ objT c_PtrCtr
  , mkStaticMethod "newGcedPtr" np $ toGcT $ ptrT $ objT c_PtrCtr
  , mkStaticMethod "resetCounters" np voidT
  , mkStaticMethod "getConstructionCount" np intT
  , mkStaticMethod "getDestructionCount" np intT
  , mkConstMethod "redButton" np voidT
  ]

c_PtrCtrWithToHeapConversion :: Class
c_PtrCtrWithToHeapConversion =
  addReqIncludes [includeStd "ptrctr.hpp"] $
  classSetConversionToHeap $
  classAddFeatures [Copyable] $
  makeClass (ident "PtrCtr") (Just $ toExtName "PtrCtrWithToHeapConversion") []
  [ mkCtor "new" np
  , mkStaticMethod' "newGcedObj" "newHeapObj" np $ objT c_PtrCtrWithToHeapConversion
  , mkStaticMethod "resetCounters" np voidT
  , mkStaticMethod "getConstructionCount" np intT
  , mkStaticMethod "getDestructionCount" np intT
  ]

c_PtrCtrWithToGcConversion :: Class
c_PtrCtrWithToGcConversion =
  addReqIncludes [includeStd "ptrctr.hpp"] $
  classSetConversionToGc $
  classAddFeatures [Copyable] $
  makeClass (ident "PtrCtr") (Just $ toExtName "PtrCtrWithToGcConversion") []
  [ mkCtor "new" np
  , mkStaticMethod "newGcedObj" np $ objT c_PtrCtrWithToGcConversion
  , mkStaticMethod "resetCounters" np voidT
  , mkStaticMethod "getConstructionCount" np intT
  , mkStaticMethod "getDestructionCount" np intT
  ]

c_ClassWithAltPrefix :: Class
c_ClassWithAltPrefix =
  addReqIncludes [includeStd "class-prefixes.hpp"] $
  classSetEntityPrefix "AltPrefixClass_" $
  makeClass (ident "ClassWithAltPrefix") Nothing []
  [ mkStaticMethod "foo" np intT ]

c_ClassWithNoPrefix :: Class
c_ClassWithNoPrefix =
  addReqIncludes [includeStd "class-prefixes.hpp"] $
  classSetEntityPrefix "" $
  makeClass (ident "ClassWithNoPrefix") Nothing []
  [ mkCtor "ctorWithNoPrefix" np
  , mkMethod "methodWithNoPrefix" np intT
  ]

f_piapprox :: Function
f_piapprox =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "piapprox") Nothing Pure np intT

f_piapproxNonpure :: Function
f_piapproxNonpure =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "piapprox") (Just $ toExtName "piapproxNonpure") Nonpure np intT

f_timesTwo :: Function
f_timesTwo =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "timesTwo") Nothing Pure [longT] longT

f_takesLongFn :: Function
f_takesLongFn =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "takesLongFn") Nothing Pure [ptrT $ fnT [longT] longT, longT] longT

cb_LongCallback :: Callback
cb_LongCallback =
  makeCallback (toExtName "LongCallback") [longT] longT

f_takesIntBoxFn :: Function
f_takesIntBoxFn =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "takesIntBoxFn") Nothing Nonpure
  [ptrT $ fnT [ptrT $ objT c_IntBox] $ ptrT $ objT c_IntBox, intT] intT

cb_IntBoxCallback :: Callback
cb_IntBoxCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "IntBoxCallback") [ptrT $ objT c_IntBox] $ ptrT $ objT c_IntBox

f_givePtrCtrByValue :: Function
f_givePtrCtrByValue =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "givePtrCtrByValue") Nothing Nonpure np $ objToHeapT c_PtrCtr

f_givePtrCtrByValueToCallback :: Function
f_givePtrCtrByValueToCallback =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "givePtrCtrByValueToCallback") Nothing Nonpure
  [callbackT cb_GetPtrCtrByValueCallback] voidT

cb_GetPtrCtrByValueCallback :: Callback
cb_GetPtrCtrByValueCallback =
  addReqIncludes [includeStd "ptrctr.hpp"] $
  makeCallback (toExtName "GetPtrCtrByValueCallback") [objToHeapT c_PtrCtr] voidT

f_getBoxValueByValue :: Function
f_getBoxValueByValue =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getBoxValueByValue") Nothing Nonpure [objT c_IntBox] intT

f_getBoxValueByRef :: Function
f_getBoxValueByRef =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getBoxValueByRef") Nothing Nonpure [refT $ objT c_IntBox] intT

f_getBoxValueByRefConst :: Function
f_getBoxValueByRefConst =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getBoxValueByRefConst") Nothing Nonpure [refT $ constT $ objT c_IntBox] intT

f_getBoxValueByPtr :: Function
f_getBoxValueByPtr =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getBoxValueByPtr") Nothing Nonpure [ptrT $ objT c_IntBox] intT

f_getBoxValueByPtrConst :: Function
f_getBoxValueByPtrConst =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getBoxValueByPtrConst") Nothing Nonpure [ptrT $ constT $ objT c_IntBox] intT

f_makeBoxByValue :: Function
f_makeBoxByValue =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "makeBoxByValue") Nothing Nonpure [intT] $ objT c_IntBox

f_makeBoxByRef :: Function
f_makeBoxByRef =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "makeBoxByRef") Nothing Nonpure [intT] $ refT $ objT c_IntBox

f_makeBoxByRefConst :: Function
f_makeBoxByRefConst =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "makeBoxByRefConst") Nothing Nonpure [intT] $ refT $ constT $ objT c_IntBox

f_makeBoxByPtr :: Function
f_makeBoxByPtr =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "makeBoxByPtr") Nothing Nonpure [intT] $ ptrT $ objT c_IntBox

f_makeBoxByPtrConst :: Function
f_makeBoxByPtrConst =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "makeBoxByPtrConst") Nothing Nonpure [intT] $ ptrT $ constT $ objT c_IntBox

cb_GetBoxValueByValueCallback :: Callback
cb_GetBoxValueByValueCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByValueCallback") [objT c_IntBox] intT

cb_GetBoxValueByRefCallback :: Callback
cb_GetBoxValueByRefCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByRefCallback") [refT $ objT c_IntBox] intT

cb_GetBoxValueByRefConstCallback :: Callback
cb_GetBoxValueByRefConstCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByRefConstCallback") [refT $ constT $ objT c_IntBox] intT

cb_GetBoxValueByPtrCallback :: Callback
cb_GetBoxValueByPtrCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByPtrCallback") [ptrT $ objT c_IntBox] intT

cb_GetBoxValueByPtrConstCallback :: Callback
cb_GetBoxValueByPtrConstCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByPtrConstCallback") [ptrT $ constT $ objT c_IntBox] intT

f_getBoxValueByValueCallbackDriver :: Function
f_getBoxValueByValueCallbackDriver =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getBoxValueByValueCallbackDriver") Nothing Nonpure
  [callbackT cb_GetBoxValueByValueCallback, intT] intT

f_getBoxValueByRefCallbackDriver :: Function
f_getBoxValueByRefCallbackDriver =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getBoxValueByRefCallbackDriver") Nothing Nonpure
  [callbackT cb_GetBoxValueByRefCallback, intT] intT

f_getBoxValueByRefConstCallbackDriver :: Function
f_getBoxValueByRefConstCallbackDriver =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getBoxValueByRefConstCallbackDriver") Nothing Nonpure
  [callbackT cb_GetBoxValueByRefConstCallback, intT] intT

f_getBoxValueByPtrCallbackDriver :: Function
f_getBoxValueByPtrCallbackDriver =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getBoxValueByPtrCallbackDriver") Nothing Nonpure
  [callbackT cb_GetBoxValueByPtrCallback, intT] intT

f_getBoxValueByPtrConstCallbackDriver :: Function
f_getBoxValueByPtrConstCallbackDriver =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getBoxValueByPtrConstCallbackDriver") Nothing Nonpure
  [callbackT cb_GetBoxValueByPtrConstCallback, intT] intT

cb_MakeBoxByValueCallback :: Callback
cb_MakeBoxByValueCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByValueCallback") [intT] $ objT c_IntBox

cb_MakeBoxByRefCallback :: Callback
cb_MakeBoxByRefCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByRefCallback") [intT] $ refT $ objT c_IntBox

cb_MakeBoxByRefConstCallback :: Callback
cb_MakeBoxByRefConstCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByRefConstCallback") [intT] $ refT $ constT $ objT c_IntBox

cb_MakeBoxByPtrCallback :: Callback
cb_MakeBoxByPtrCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByPtrCallback") [intT] $ ptrT $ objT c_IntBox

cb_MakeBoxByPtrConstCallback :: Callback
cb_MakeBoxByPtrConstCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByPtrConstCallback") [intT] $ ptrT $ constT $ objT c_IntBox

f_makeBoxByValueCallbackDriver :: Function
f_makeBoxByValueCallbackDriver =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "makeBoxByValueCallbackDriver") Nothing Nonpure
  [callbackT cb_MakeBoxByValueCallback, intT] intT

f_makeBoxByRefCallbackDriver :: Function
f_makeBoxByRefCallbackDriver =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "makeBoxByRefCallbackDriver") Nothing Nonpure
  [callbackT cb_MakeBoxByRefCallback, intT] intT

f_makeBoxByRefConstCallbackDriver :: Function
f_makeBoxByRefConstCallbackDriver =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "makeBoxByRefConstCallbackDriver") Nothing Nonpure
  [callbackT cb_MakeBoxByRefConstCallback, intT] intT

f_makeBoxByPtrCallbackDriver :: Function
f_makeBoxByPtrCallbackDriver =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "makeBoxByPtrCallbackDriver") Nothing Nonpure
  [callbackT cb_MakeBoxByPtrCallback, intT] intT

f_makeBoxByPtrConstCallbackDriver :: Function
f_makeBoxByPtrConstCallbackDriver =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "makeBoxByPtrConstCallbackDriver") Nothing Nonpure
  [callbackT cb_MakeBoxByPtrConstCallback, intT] intT

f_makeBoxToGc :: Function
f_makeBoxToGc =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "makeBoxToGc") Nothing Nonpure [intT] $ toGcT $ objT c_IntBox

cb_takesBoxToGcCallback :: Callback
cb_takesBoxToGcCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  makeCallback (toExtName "TakesBoxToGcCallback") [toGcT $ objT c_IntBox] intT

f_callBoxToGcCallback :: Function
f_callBoxToGcCallback =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "callBoxToGcCallback") Nothing Nonpure
  [callbackT cb_takesBoxToGcCallback, intT] intT

c_IntBoxWithFnMethods :: Class
c_IntBoxWithFnMethods =
  addReqIncludes [includeStd "functions.hpp", includeStd "intbox.hpp"] $
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
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "isTrue") Nothing Nonpure [boolT] boolT

f_isFalse :: Function
f_isFalse =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "isFalse") Nothing Nonpure [boolT] boolT

f_sizeOfBool :: Function
f_sizeOfBool =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "sizeOfBool") Nothing Pure np sizeT

f_sizeOfChar :: Function
f_sizeOfChar =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "sizeOfChar") Nothing Pure np sizeT

f_sizeOfShort :: Function
f_sizeOfShort =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "sizeOfShort") Nothing Pure np sizeT

f_sizeOfInt :: Function
f_sizeOfInt =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "sizeOfInt") Nothing Pure np sizeT

f_sizeOfLong :: Function
f_sizeOfLong =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "sizeOfLong") Nothing Pure np sizeT

f_sizeOfLLong :: Function
f_sizeOfLLong =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "sizeOfLLong") Nothing Pure np sizeT

f_sizeOfFloat :: Function
f_sizeOfFloat =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "sizeOfFloat") Nothing Pure np sizeT

f_sizeOfDouble :: Function
f_sizeOfDouble =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "sizeOfDouble") Nothing Pure np sizeT

f_sizeOfPtrdiff :: Function
f_sizeOfPtrdiff =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "sizeOfPtrdiff") Nothing Pure np sizeT

f_sizeOfSize :: Function
f_sizeOfSize =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "sizeOfSize") Nothing Pure np sizeT

f_sizeOfSSize :: Function
f_sizeOfSSize =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "sizeOfSSize") Nothing Pure np sizeT

f_doubleInt :: Function
f_doubleInt =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleInt") Nothing Pure [intT] intT

f_doubleLong :: Function
f_doubleLong =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleLong") Nothing Pure [longT] longT

f_doubleFloat :: Function
f_doubleFloat =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleFloat") Nothing Pure [floatT] floatT

f_doubleDouble :: Function
f_doubleDouble =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleDouble") Nothing Pure [doubleT] doubleT

f_doubleInt8 :: Function
f_doubleInt8 =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleInt8") Nothing Pure [int8T] int8T

f_doubleInt32 :: Function
f_doubleInt32 =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleInt32") Nothing Pure [int32T] int32T

f_doubleUInt16 :: Function
f_doubleUInt16 =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleUInt16") Nothing Pure [word16T] word16T

f_doubleUInt64 :: Function
f_doubleUInt64 =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleUInt64") Nothing Pure [word64T] word64T

f_getBoolPtr :: Function
f_getBoolPtr =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getBoolPtr") Nothing Nonpure np $ ptrT boolT

f_getIntPtr :: Function
f_getIntPtr =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getIntPtr") Nothing Nonpure np $ ptrT intT

f_getIntPtrPtr :: Function
f_getIntPtrPtr =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getIntPtrPtr") Nothing Nonpure np $ ptrT $ ptrT intT

f_getIntBoxPtrPtr :: Function
f_getIntBoxPtrPtr =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "getIntBoxPtrPtr") Nothing Nonpure np $ ptrT $ ptrT $ objT c_IntBox

f_doubleIntPtr :: Function
f_doubleIntPtr =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleIntPtr") Nothing Nonpure [ptrT intT] voidT

f_doubleIntPtrPtr :: Function
f_doubleIntPtrPtr =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleIntPtrPtr") Nothing Nonpure [ptrT $ ptrT intT] voidT

f_doubleIntRef :: Function
f_doubleIntRef =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleIntRef") Nothing Nonpure [refT intT] voidT

f_doubleIntBoxPtrPtr :: Function
f_doubleIntBoxPtrPtr =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "doubleIntBoxPtrPtr") Nothing Nonpure [ptrT $ ptrT $ objT c_IntBox] voidT

c_Undeletable :: Class
c_Undeletable =
  addReqIncludes [includeStd "undeletable.hpp"] $
  classSetDtorPrivate $
  makeClass (ident "Undeletable") Nothing []
  [ mkStaticMethod "getInstance" np $ refT $ objT c_Undeletable
  ]

c_InheritanceA :: Class
c_InheritanceA =
  addReqIncludes [includeStd "inheritance.hpp"] $
  makeClass (ident "InheritanceA") Nothing []
  [ mkCtor "new" np
  , mkConstMethod "aFoo" np intT
  , mkConstMethod "aBar" np intT
  ]

c_InheritanceB :: Class
c_InheritanceB =
  addReqIncludes [includeStd "inheritance.hpp"] $
  makeClass (ident "InheritanceB") Nothing []
  [ mkConstMethod "bFoo" np intT
  ]

c_InheritanceC :: Class
c_InheritanceC =
  addReqIncludes [includeStd "inheritance.hpp"] $
  makeClass (ident "InheritanceC") Nothing [c_InheritanceA, c_InheritanceB]
  [ mkCtor "new" np
  ]

e_BetterBool :: CppEnum
e_BetterBool =
  addReqIncludes [includeStd "enum.hpp"] $
  enumSetNoUnknownValueEntry $
  makeEnum (ident "BetterBool") Nothing
  [ (0, ["true"])
  , (1, ["false"])
  , (4, ["file", "not", "found"])
  ]

f_betterBoolId :: Function
f_betterBoolId =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "betterBoolId") Nothing Pure [enumT e_BetterBool] $ enumT e_BetterBool

cb_BetterBoolCallback :: Callback
cb_BetterBoolCallback =
  addReqIncludes [includeStd "enum.hpp"] $
  makeCallback (toExtName "BetterBoolCallback") [enumT e_BetterBool] $ enumT e_BetterBool

f_takesBetterBoolCallback :: Function
f_takesBetterBoolCallback =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "takesBetterBoolCallback") Nothing Pure
  [callbackT cb_BetterBoolCallback, enumT e_BetterBool] $ enumT e_BetterBool

c_BaseException :: Class
c_BaseException =
  addReqIncludes [includeStd "exceptions.hpp"] $
  classMakeException $
  classAddFeatures [Copyable] $
  makeClass (ident "BaseException") Nothing []
  [ mkCtor "new" np ]

c_FileException :: Class
c_FileException =
  addReqIncludes [includeStd "exceptions.hpp"] $
  classMakeException $
  classAddFeatures [Copyable] $
  makeClass (ident "FileException") Nothing [c_BaseException]
  [ mkCtor "new" np ]

c_ReadException :: Class
c_ReadException =
  addReqIncludes [includeStd "exceptions.hpp"] $
  classMakeException $
  classAddFeatures [Copyable] $
  makeClass (ident "ReadException") Nothing [c_FileException]
  [ mkCtor "new" np ]

c_WriteException :: Class
c_WriteException =
  addReqIncludes [includeStd "exceptions.hpp"] $
  classMakeException $
  classAddFeatures [Copyable] $
  makeClass (ident "WriteException") Nothing [c_FileException]
  [ mkCtor "new" np ]

f_throwsBaseException :: Function
f_throwsBaseException =
  addReqIncludes [includeStd "functions.hpp"] $
  handleExceptions [CatchClass c_BaseException] $
  makeFn (ident "throwsBaseException") Nothing Nonpure np voidT

f_throwsFileException :: Function
f_throwsFileException =
  addReqIncludes [includeStd "functions.hpp"] $
  handleExceptions [CatchClass c_FileException] $
  makeFn (ident "throwsFileException") Nothing Nonpure np voidT

f_throwsReadException :: Function
f_throwsReadException =
  addReqIncludes [includeStd "functions.hpp"] $
  handleExceptions [CatchClass c_ReadException] $
  makeFn (ident "throwsReadException") Nothing Nonpure np voidT

f_throwsWriteException :: Function
f_throwsWriteException =
  addReqIncludes [includeStd "functions.hpp"] $
  handleExceptions [CatchClass c_WriteException] $
  makeFn (ident "throwsWriteException") Nothing Nonpure np voidT

f_throwsPtrCtr :: Function
f_throwsPtrCtr =
  addReqIncludes [includeStd "functions.hpp"] $
  handleExceptions [CatchClass c_PtrCtr] $
  makeFn (ident "throwsPtrCtr") Nothing Nonpure np voidT

f_throwsAny :: Function
f_throwsAny =
  addReqIncludes [includeStd "functions.hpp"] $
  handleExceptions [CatchAll] $
  makeFn (ident "throwsAny") Nothing Nonpure [intT] voidT

cb_ThrowingCallback :: Callback
cb_ThrowingCallback =
  callbackSetThrows True $
  makeCallback (toExtName "ThrowingCallback") np voidT

f_invokeThrowingCallback :: Function
f_invokeThrowingCallback =
  addReqIncludes [includeStd "functions.hpp"] $
  makeFn (ident "invokeThrowingCallback") Nothing Nonpure
  [callbackT cb_ThrowingCallback] intT

-- This ensures that generated exception handling code plays well with the
-- conversions necessary for returning booleans from a function.
f_throwingReturnBool :: Function
f_throwingReturnBool =
  addReqIncludes [includeStd "functions.hpp"] $
  handleExceptions [CatchClass c_BaseException] $
  makeFn (ident "throwingReturnBool") Nothing Nonpure np boolT

-- This ensures that generated exception handling code plays well with generated
-- code that returns ints from functions -- ints require no conversions, unlike
-- other types.
f_throwingReturnInt :: Function
f_throwingReturnInt =
  addReqIncludes [includeStd "functions.hpp"] $
  handleExceptions [CatchClass c_BaseException] $
  makeFn (ident "throwingReturnInt") Nothing Nonpure np intT

-- This ensures that generated exception handling code plays well with the
-- conversions necessary for returning objects from a function.
f_throwingReturnIntBox :: Function
f_throwingReturnIntBox =
  addReqIncludes [includeStd "functions.hpp"] $
  handleExceptions [CatchClass c_BaseException] $
  makeFn (ident "throwingReturnIntBox") Nothing Nonpure np $ objT c_IntBox

-- This ensures that generated exception handling code plays well with the
-- conversions necessary for returning objects from a callback.
cb_ThrowingMakeBoxByValueCallback :: Callback
cb_ThrowingMakeBoxByValueCallback =
  addReqIncludes [includeStd "intbox.hpp"] $
  callbackSetThrows True $
  makeCallback (toExtName "ThrowingMakeBoxByValueCallback") [intT] $ objT c_IntBox

f_throwingMakeBoxByValueCallbackDriver :: Function
f_throwingMakeBoxByValueCallbackDriver =
  addReqIncludes [includeStd "functions.hpp"] $
  handleExceptions [CatchClass c_BaseException] $
  makeFn (ident "throwingMakeBoxByValueCallbackDriver") Nothing Nonpure
  [callbackT cb_ThrowingMakeBoxByValueCallback, intT] intT
