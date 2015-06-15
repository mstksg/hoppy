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
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = HsTyCon $ UnQual $ HsIdent "CppopP.String"
             , classHaskellConversionTypeImports = hsImportForPrelude
             , classHaskellConversionToCppFn =
               "CppopP.flip CppopFC.withCString stdString_newFromCString"
             , classHaskellConversionToCppImports = hsImportForPrelude `mappend` hsImportForForeignC
             , classHaskellConversionFromCppFn = "CppopFC.peekCString <=< stdString_c_str"
             , classHaskellConversionFromCppImports =
               hsImport1 "Control.Monad" "(<=<)" `mappend` hsImportForForeignC
             }
           }) $
  makeClass (ident1 "std" "string") (Just $ toExtName "StdString")
  []
  [ mkCtor c_std__string "newFromCString" [TPtr $ TConst TChar]
  ]
  [ mkConstMethod c_std__string "c_str" [] $ TPtr $ TConst TChar
  , mkConstMethod c_std__string "size" [] TSize
    -- TODO Automate these with "class traits".
  , mkConstMethod c_std__string OpEq [TObj c_std__string] TBool
  , mkConstMethod c_std__string OpNe [TObj c_std__string] TBool
  , mkConstMethod c_std__string OpLt [TObj c_std__string] TBool
  , mkConstMethod c_std__string OpLe [TObj c_std__string] TBool
  , mkConstMethod c_std__string OpGt [TObj c_std__string] TBool
  , mkConstMethod c_std__string OpGe [TObj c_std__string] TBool
  ]
