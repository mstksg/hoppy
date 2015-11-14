-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE CPP #-}

-- | Bindings for @std::string@.
module Foreign.Hoppy.Generator.Std.String (c_string) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell.General (addImports, sayLn)
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.ClassFeature
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
               return $ HsTyCon $ UnQual $ HsIdent "HoppyP.String"
             , classHaskellConversionToCppFn = do
               addImports $ mconcat [hsImportForPrelude, hsImportForForeignC]
               sayLn "HoppyP.flip HoppyFC.withCString stdString_newFromCString"
             , classHaskellConversionFromCppFn = do
               addImports $ mconcat [hsImport1 "Control.Monad" "(<=<)", hsImportForForeignC]
               sayLn "HoppyFC.peekCString <=< stdString_c_str"
             }
           }) $
  makeClass (ident1 "std" "string") (Just $ toExtName "StdString")
  []
  [ mkCtor "new" []
  , mkCtor "newFromCString" [TPtr $ TConst TChar]
  ]
  [ mkConstMethod' "at" "at" [TInt] $ TRef TChar
  , mkConstMethod' "at" "get" [TInt] TChar
  , mkConstMethod "c_str" [] $ TPtr $ TConst TChar
  , mkConstMethod "size" [] TSize
  ]
