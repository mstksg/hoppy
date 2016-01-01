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

module Main (main) where

import Foreign.Hoppy.Generator.Main (run)
import Foreign.Hoppy.Generator.Language.Haskell (addImports, sayLn)
import Foreign.Hoppy.Generator.Spec
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = case interfaceResult of
  Left errorMsg -> do
    putStrLn $ "Error initializing interface: " ++ errorMsg
    exitFailure
  Right iface -> do
    args <- getArgs
    _ <- run [iface] args
    return ()

interfaceResult :: Either String Interface
interfaceResult =
  interfaceAddHaskellModuleBase ["Foreign", "Hoppy", "Test"] =<<
  interface "test" modules

modules :: [Module]
modules = [testModule]

testModule :: Module
testModule =
  moduleModify' (makeModule "basic" "basic.hpp" "basic.cpp") $
  moduleAddExports
  [ -- Is this thing on?
    ExportClass c_IntBox
  , ExportClass c_PtrCtr
  , ExportFn f_piapprox
  , ExportFn f_piapproxNonpure
  , ExportFn f_timesTwo
    -- For testing TObjToHeap.
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
    -- Multiple inheritance tests.
  , ExportClass c_InheritanceA
  , ExportClass c_InheritanceB
  , ExportClass c_InheritanceC
  ]

c_IntBox :: Class
c_IntBox =
  addReqIncludes [includeLocal "intbox.hpp"] $
  classModifyConversion
  (\c -> c { classHaskellConversion = Just ClassHaskellConversion
             { classHaskellConversionType = do
               addImports $ hsWholeModuleImport "Foreign.Hoppy.Test.Basic.HsBox"
               return $ HsTyCon $ UnQual $ HsIdent "HsBox"
             , classHaskellConversionToCppFn = sayLn "intBox_newWithValue . getHsBox"
             , classHaskellConversionFromCppFn = do
               addImports $ hsImports "Prelude" ["(.)", "fmap"]
               sayLn "fmap HsBox . intBox_get"
             }
           }) $
  makeClass (ident "IntBox") Nothing []
  [ mkCtor "new" []
  , mkCtor "newWithValue" [TInt]
  ]
  [ mkConstMethod "get" [] TInt
  , mkMethod "set" [TInt] TVoid
  ]

c_PtrCtr :: Class
c_PtrCtr =
  addReqIncludes [includeLocal "ptrctr.hpp"] $
  makeClass (ident "PtrCtr") Nothing []
  [ mkCtor "new" [] ]
  [ mkStaticMethod "resetCounters" [] TVoid
  , mkStaticMethod "getConstructionCount" [] TInt
  , mkStaticMethod "getDestructionCount" [] TInt
  ]

f_piapprox :: Function
f_piapprox =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "piapprox") Nothing Pure [] TInt

f_piapproxNonpure :: Function
f_piapproxNonpure =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "piapprox") (Just $ toExtName "piapproxNonpure") Nonpure [] TInt

f_timesTwo :: Function
f_timesTwo =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "timesTwo") Nothing Pure [TLong] TLong

f_givePtrCtrByValue :: Function
f_givePtrCtrByValue =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "givePtrCtrByValue") Nothing Nonpure [] $ TObjToHeap c_PtrCtr

f_givePtrCtrByValueToCallback :: Function
f_givePtrCtrByValueToCallback =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "givePtrCtrByValueToCallback") Nothing Nonpure
  [TCallback cb_GetPtrCtrByValueCallback] TVoid

cb_GetPtrCtrByValueCallback :: Callback
cb_GetPtrCtrByValueCallback =
  addReqIncludes [includeLocal "ptrctr.hpp"] $
  makeCallback (toExtName "GetPtrCtrByValueCallback") [TObjToHeap c_PtrCtr] TVoid

f_getBoxValueByValue :: Function
f_getBoxValueByValue =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByValue") Nothing Nonpure [TObj c_IntBox] TInt

f_getBoxValueByRef :: Function
f_getBoxValueByRef =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByRef") Nothing Nonpure [TRef $ TObj c_IntBox] TInt

f_getBoxValueByRefConst :: Function
f_getBoxValueByRefConst =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByRefConst") Nothing Nonpure [TRef $ TConst $ TObj c_IntBox] TInt

f_getBoxValueByPtr :: Function
f_getBoxValueByPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByPtr") Nothing Nonpure [TPtr $ TObj c_IntBox] TInt

f_getBoxValueByPtrConst :: Function
f_getBoxValueByPtrConst =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByPtrConst") Nothing Nonpure [TPtr $ TConst $ TObj c_IntBox] TInt

f_makeBoxByValue :: Function
f_makeBoxByValue =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByValue") Nothing Nonpure [TInt] $ TObj c_IntBox

f_makeBoxByRef :: Function
f_makeBoxByRef =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByRef") Nothing Nonpure [TInt] $ TRef $ TObj c_IntBox

f_makeBoxByRefConst :: Function
f_makeBoxByRefConst =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByRefConst") Nothing Nonpure [TInt] $ TRef $ TConst $ TObj c_IntBox

f_makeBoxByPtr :: Function
f_makeBoxByPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByPtr") Nothing Nonpure [TInt] $ TPtr $ TObj c_IntBox

f_makeBoxByPtrConst :: Function
f_makeBoxByPtrConst =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByPtrConst") Nothing Nonpure [TInt] $ TPtr $ TConst $ TObj c_IntBox

cb_GetBoxValueByValueCallback :: Callback
cb_GetBoxValueByValueCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByValueCallback") [TObj c_IntBox] TInt

cb_GetBoxValueByRefCallback :: Callback
cb_GetBoxValueByRefCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByRefCallback") [TRef $ TObj c_IntBox] TInt

cb_GetBoxValueByRefConstCallback :: Callback
cb_GetBoxValueByRefConstCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByRefConstCallback") [TRef $ TConst $ TObj c_IntBox] TInt

cb_GetBoxValueByPtrCallback :: Callback
cb_GetBoxValueByPtrCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByPtrCallback") [TPtr $ TObj c_IntBox] TInt

cb_GetBoxValueByPtrConstCallback :: Callback
cb_GetBoxValueByPtrConstCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "GetBoxValueByPtrConstCallback") [TPtr $ TConst $ TObj c_IntBox] TInt

f_getBoxValueByValueCallbackDriver :: Function
f_getBoxValueByValueCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByValueCallbackDriver") Nothing Nonpure
  [TCallback cb_GetBoxValueByValueCallback, TInt] TInt

f_getBoxValueByRefCallbackDriver :: Function
f_getBoxValueByRefCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByRefCallbackDriver") Nothing Nonpure
  [TCallback cb_GetBoxValueByRefCallback, TInt] TInt

f_getBoxValueByRefConstCallbackDriver :: Function
f_getBoxValueByRefConstCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByRefConstCallbackDriver") Nothing Nonpure
  [TCallback cb_GetBoxValueByRefConstCallback, TInt] TInt

f_getBoxValueByPtrCallbackDriver :: Function
f_getBoxValueByPtrCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByPtrCallbackDriver") Nothing Nonpure
  [TCallback cb_GetBoxValueByPtrCallback, TInt] TInt

f_getBoxValueByPtrConstCallbackDriver :: Function
f_getBoxValueByPtrConstCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoxValueByPtrConstCallbackDriver") Nothing Nonpure
  [TCallback cb_GetBoxValueByPtrConstCallback, TInt] TInt

cb_MakeBoxByValueCallback :: Callback
cb_MakeBoxByValueCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByValueCallback") [TInt] $ TObj c_IntBox

cb_MakeBoxByRefCallback :: Callback
cb_MakeBoxByRefCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByRefCallback") [TInt] $ TRef $ TObj c_IntBox

cb_MakeBoxByRefConstCallback :: Callback
cb_MakeBoxByRefConstCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByRefConstCallback") [TInt] $ TRef $ TConst $ TObj c_IntBox

cb_MakeBoxByPtrCallback :: Callback
cb_MakeBoxByPtrCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByPtrCallback") [TInt] $ TPtr $ TObj c_IntBox

cb_MakeBoxByPtrConstCallback :: Callback
cb_MakeBoxByPtrConstCallback =
  addReqIncludes [includeLocal "intbox.hpp"] $
  makeCallback (toExtName "MakeBoxByPtrConstCallback") [TInt] $ TPtr $ TConst $ TObj c_IntBox

f_makeBoxByValueCallbackDriver :: Function
f_makeBoxByValueCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByValueCallbackDriver") Nothing Nonpure
  [TCallback cb_MakeBoxByValueCallback, TInt] TInt

f_makeBoxByRefCallbackDriver :: Function
f_makeBoxByRefCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByRefCallbackDriver") Nothing Nonpure
  [TCallback cb_MakeBoxByRefCallback, TInt] TInt

f_makeBoxByRefConstCallbackDriver :: Function
f_makeBoxByRefConstCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByRefConstCallbackDriver") Nothing Nonpure
  [TCallback cb_MakeBoxByRefConstCallback, TInt] TInt

f_makeBoxByPtrCallbackDriver :: Function
f_makeBoxByPtrCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByPtrCallbackDriver") Nothing Nonpure
  [TCallback cb_MakeBoxByPtrCallback, TInt] TInt

f_makeBoxByPtrConstCallbackDriver :: Function
f_makeBoxByPtrConstCallbackDriver =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "makeBoxByPtrConstCallbackDriver") Nothing Nonpure
  [TCallback cb_MakeBoxByPtrConstCallback, TInt] TInt

c_IntBoxWithFnMethods :: Class
c_IntBoxWithFnMethods =
  addReqIncludes [includeLocal "functions.hpp", includeLocal "intbox.hpp"] $
  makeClass (ident "IntBox") (Just $ toExtName "IntBoxWithFnMethods") []
  [ mkCtor "new" [TInt]
  ]
  [ -- A normal method.
    makeFnMethod (ident "getBoxValueByRef") "getValue" MNormal Nonpure
    [TRef $ TObj c_IntBoxWithFnMethods] TInt
    -- A const method.
  , makeFnMethod (ident "getBoxValueByPtrConst") "getValueConst" MConst Nonpure
    [TPtr $ TConst $ TObj c_IntBoxWithFnMethods] TInt
    -- A static method.
  , makeFnMethod (ident "timesTwo") "double" MStatic Pure [TLong] TLong
  ]

f_isTrue :: Function
f_isTrue =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "isTrue") Nothing Nonpure [TBool] TBool

f_isFalse :: Function
f_isFalse =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "isFalse") Nothing Nonpure [TBool] TBool

f_sizeOfBool :: Function
f_sizeOfBool =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfBool") Nothing Pure [] TSize

f_sizeOfChar :: Function
f_sizeOfChar =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfChar") Nothing Pure [] TSize

f_sizeOfShort :: Function
f_sizeOfShort =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfShort") Nothing Pure [] TSize

f_sizeOfInt :: Function
f_sizeOfInt =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfInt") Nothing Pure [] TSize

f_sizeOfLong :: Function
f_sizeOfLong =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfLong") Nothing Pure [] TSize

f_sizeOfLLong :: Function
f_sizeOfLLong =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfLLong") Nothing Pure [] TSize

f_sizeOfFloat :: Function
f_sizeOfFloat =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfFloat") Nothing Pure [] TSize

f_sizeOfDouble :: Function
f_sizeOfDouble =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfDouble") Nothing Pure [] TSize

f_sizeOfPtrdiff :: Function
f_sizeOfPtrdiff =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfPtrdiff") Nothing Pure [] TSize

f_sizeOfSize :: Function
f_sizeOfSize =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfSize") Nothing Pure [] TSize

f_sizeOfSSize :: Function
f_sizeOfSSize =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "sizeOfSSize") Nothing Pure [] TSize

f_doubleInt :: Function
f_doubleInt =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleInt") Nothing Pure [TInt] TInt

f_doubleLong :: Function
f_doubleLong =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleLong") Nothing Pure [TLong] TLong

f_doubleFloat :: Function
f_doubleFloat =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleFloat") Nothing Pure [TFloat] TFloat

f_doubleDouble :: Function
f_doubleDouble =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleDouble") Nothing Pure [TDouble] TDouble

f_doubleInt8 :: Function
f_doubleInt8 =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleInt8") Nothing Pure [TInt8] TInt8

f_doubleInt32 :: Function
f_doubleInt32 =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleInt32") Nothing Pure [TInt32] TInt32

f_doubleUInt16 :: Function
f_doubleUInt16 =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleUInt16") Nothing Pure [TWord16] TWord16

f_doubleUInt64 :: Function
f_doubleUInt64 =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleUInt64") Nothing Pure [TWord64] TWord64

f_getBoolPtr :: Function
f_getBoolPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getBoolPtr") Nothing Nonpure [] $ TPtr TBool

f_getIntPtr :: Function
f_getIntPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getIntPtr") Nothing Nonpure [] $ TPtr TInt

f_getIntPtrPtr :: Function
f_getIntPtrPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getIntPtrPtr") Nothing Nonpure [] $ TPtr $ TPtr TInt

f_getIntBoxPtrPtr :: Function
f_getIntBoxPtrPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "getIntBoxPtrPtr") Nothing Nonpure [] $ TPtr $ TPtr $ TObj c_IntBox

f_doubleIntPtr :: Function
f_doubleIntPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleIntPtr") Nothing Nonpure [TPtr TInt] TVoid

f_doubleIntPtrPtr :: Function
f_doubleIntPtrPtr =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleIntPtrPtr") Nothing Nonpure [TPtr $ TPtr TInt] TVoid

f_doubleIntRef :: Function
f_doubleIntRef =
  addReqIncludes [includeLocal "functions.hpp"] $
  makeFn (ident "doubleIntRef") Nothing Nonpure [TRef TInt] TVoid

c_InheritanceA :: Class
c_InheritanceA =
  addReqIncludes [includeLocal "inheritance.hpp"] $
  makeClass (ident "InheritanceA") Nothing []
  [ mkCtor "new" [] ]
  [ mkConstMethod "aFoo" [] TInt
  , mkConstMethod "aBar" [] TInt
  ]

c_InheritanceB :: Class
c_InheritanceB =
  addReqIncludes [includeLocal "inheritance.hpp"] $
  makeClass (ident "InheritanceB") Nothing [] []
  [ mkConstMethod "bFoo" [] TInt
  ]

c_InheritanceC :: Class
c_InheritanceC =
  addReqIncludes [includeLocal "inheritance.hpp"] $
  makeClass (ident "InheritanceC") Nothing [c_InheritanceA, c_InheritanceB]
  [ mkCtor "new" [] ]
  []
