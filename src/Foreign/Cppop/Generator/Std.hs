module Foreign.Cppop.Generator.Std (
  mod_std,
  c_std__string,
  ) where

import Data.Monoid (mappend)
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
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TChar
           , classCppDecoder = Just $ CppCoderFn (ident1 "std" "string") $
                               reqInclude $ includeStd "string"
           , classCppDecodeThenFree = True
           , classCppEncoder = Just $ CppCoderExpr [Just "strdup(", Nothing, Just ".c_str())"] $
                               reqInclude $ includeStd "cstring"
           , classHaskellType =
             Just HaskellEncoding
             { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "CppopP.String"
             , haskellEncodingCType = HsTyCon $ UnQual $ HsIdent "CppopFC.CString"
             , haskellEncodingDecoder = "CppopFCRS.decodeAndFreeCString"
             , haskellEncodingEncoder = "CppopFC.newCString"
             , haskellEncodingTypeImports = hsImportForPrelude
             , haskellEncodingCTypeImports = hsImportForForeignC
             , haskellEncodingFnImports = hsImportForForeignC `mappend` hsImportForSupport
             }
           }) $
  makeClass (ident1 "std" "string") (Just $ toExtName "StdString")
  []
  []
  [ makeMethod "size" (toExtName "string_size") MConst Nonpure [] TSize ]
