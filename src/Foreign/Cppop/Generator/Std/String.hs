module Foreign.Cppop.Generator.Std.String (c_string) where

import Data.Monoid (mappend)
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Spec.ClassFeature
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

-- | @std::string@
c_string :: Class
c_string =
  addReqIncludes [includeStd "string"] $
  classAddFeatures [Assignable, Comparable, Copyable, Equatable] $
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
  [ mkCtor c_string "newFromCString" [TPtr $ TConst TChar]
  ]
  [ mkConstMethod c_string "c_str" [] $ TPtr $ TConst TChar
  , mkConstMethod c_string "size" [] TSize
  ]
