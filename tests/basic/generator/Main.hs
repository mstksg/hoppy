module Main where

import Data.Monoid (mempty)
import Foreign.Cppop.Generator.Main (run)
import Foreign.Cppop.Generator.Language.Haskell.General (addImports, sayLn)
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std (mod_std)
import Foreign.Cppop.Generator.Std.String (c_string)
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
    run [iface] args
    return ()

interfaceResult :: Either String Interface
interfaceResult =
  addInterfaceHaskellModuleBase ["Foreign", "Cppop", "Test"] =<<
  interface "test" modules

modules :: [Module]
modules = [testModule]

testModule :: Module
testModule =
  modifyModule' (makeModule "basic" "basic.hpp" "basic.cpp") $
  addModuleExports
  [ ExportClass c_IntBox
  , ExportClass c_PtrCtr
  , ExportFn f_piapprox
  , ExportFn f_piapproxNonpure
  , ExportFn f_timesTwo
  , ExportFn f_givePtrCtrByValue
  , ExportFn f_givePtrCtrByValueToCallback
  , ExportCallback cb_GetPtrCtrByValueCallback
  , ExportFn f_getBoxValueByValue
  , ExportFn f_getBoxValueByRef
  , ExportFn f_getBoxValueByRefConst
  , ExportFn f_getBoxValueByPtr
  , ExportFn f_getBoxValueByPtrConst
  , ExportFn f_makeBoxByValue
  , ExportFn f_makeBoxByRef
  , ExportFn f_makeBoxByRefConst
  , ExportFn f_makeBoxByPtr
  , ExportFn f_makeBoxByPtrConst
  , ExportCallback cb_GetBoxValueByValueCallback
  , ExportCallback cb_GetBoxValueByRefCallback
  , ExportCallback cb_GetBoxValueByRefConstCallback
  , ExportCallback cb_GetBoxValueByPtrCallback
  , ExportCallback cb_GetBoxValueByPtrConstCallback
  , ExportFn f_getBoxValueByValueCallbackDriver
  , ExportFn f_getBoxValueByRefCallbackDriver
  , ExportFn f_getBoxValueByRefConstCallbackDriver
  , ExportFn f_getBoxValueByPtrCallbackDriver
  , ExportFn f_getBoxValueByPtrConstCallbackDriver
  , ExportCallback cb_MakeBoxByValueCallback
  , ExportCallback cb_MakeBoxByRefCallback
  , ExportCallback cb_MakeBoxByRefConstCallback
  , ExportCallback cb_MakeBoxByPtrCallback
  , ExportCallback cb_MakeBoxByPtrConstCallback
  , ExportFn f_makeBoxByValueCallbackDriver
  , ExportFn f_makeBoxByRefCallbackDriver
  , ExportFn f_makeBoxByRefConstCallbackDriver
  , ExportFn f_makeBoxByPtrCallbackDriver
  , ExportFn f_makeBoxByPtrConstCallbackDriver
  ]

c_IntBox :: Class
c_IntBox =
  addReqIncludes [includeLocal "intbox.hpp"] $
  classModifyConversions
  (\c -> c { classHaskellConversion = Just ClassHaskellConversion
             { classHaskellConversionType = do
               addImports $ hsWholeModuleImport "Foreign.Cppop.Test.Basic.HsBox"
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

-- For testing TObjToHeap:

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

-- Passing objects to C++:

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

-- Returning objects from C++:

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

-- Passing objects to Haskell callbacks:

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

-- ...and the C++ drivers for the above:

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

-- Returning objects from Haskell callbacks:

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

-- ...and the C++ drivers for the above:

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
