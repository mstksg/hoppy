module Foreign.Cppop.Generator.Std (
  mod_std,
  c_std__string,
  ) where

import Foreign.Cppop.Generator.Spec
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

mod_std :: Module
mod_std = modifyModule' (makeModule "std" "std.hpp" "std.cpp") $
  addModuleExports [ExportClass c_std__string]

c_std__string :: Class
c_std__string =
  addReqIncludes [includeStd "string"] $
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = HsTyCon $ UnQual $ HsIdent "CppopP.String"
             , classHaskellConversionTypeImports = hsImportForPrelude
             , classHaskellConversionFromCppFn = "CppopFCRS.decodeAndFreeStdString"
             , classHaskellConversionFromCppImports = hsImportForForeignC
             , classHaskellConversionToCppFn = "CppopFCRS.newStdString"
             , classHaskellConversionToCppImports = hsImportForForeignC
             }
           }) $
  makeClass (ident1 "std" "string") (Just $ toExtName "StdString")
  []
  []
  [ makeMethod "size" (toExtName "string_size") MConst Nonpure [] TSize ]
