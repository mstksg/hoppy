module Foreign.Cppop.Generator.Std.String (c_string) where

import Data.Monoid (mconcat)
import Foreign.Cppop.Generator.Language.Haskell.General (addImports, sayLn)
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
             { classHaskellConversionType = do
               addImports hsImportForPrelude
               return $ HsTyCon $ UnQual $ HsIdent "CppopP.String"
             , classHaskellConversionToCppFn = do
               addImports $ mconcat [hsImportForPrelude, hsImportForForeignC]
               sayLn "CppopP.flip CppopFC.withCString stdString_newFromCString"
             , classHaskellConversionFromCppFn = do
               addImports $ mconcat [hsImport1 "Control.Monad" "(<=<)", hsImportForForeignC]
               sayLn "CppopFC.peekCString <=< stdString_c_str"
             }
           }) $
  makeClass (ident1 "std" "string") (Just $ toExtName "StdString")
  []
  [ mkCtor "newFromCString" [TPtr $ TConst TChar]
  ]
  [ mkConstMethod' "at" "at" [TInt] $ TRef TChar
  , mkConstMethod' "at" "get" [TInt] TChar
  , mkConstMethod "c_str" [] $ TPtr $ TConst TChar
  , mkConstMethod "size" [] TSize
  ]
