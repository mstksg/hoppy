-- This file is part of Hoppy.
--
-- Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
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
import Foreign.Hoppy.Generator.Language.Haskell (addExport, addImports, indent, sayLn)
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Types
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
  addAddendumHaskell addendum $
  classSetHaskellConversion
    ClassHaskellConversion
      { classHaskellConversionType = Just $ do
        addImports hsImportForPrelude
        return $ HsTyCon $ UnQual $ HsIdent "HoppyP.String"
      , classHaskellConversionToCppFn = Just $ do
        addImports $ mconcat [hsImportForPrelude, hsImportForForeignC]
        sayLn "HoppyP.flip HoppyFC.withCStringLen stdString_newFromCStringLen"
      , classHaskellConversionFromCppFn = Just $ do
        addImports $ mconcat [hsImport1 "Control.Applicative" "(<*)",
                              hsImportForForeignC,
                              hsImportForPrelude,
                              hsImportForRuntime]
        sayLn "\\s -> do"
        indent $ do
          sayLn "p <- stdString_data s"
          sayLn "n <- stdString_size s"
          sayLn "HoppyFC.peekCStringLen (p, HoppyP.fromIntegral n) <* HoppyFHR.touchCppPtr s"
      } $
  makeClass (ident1 "std" "string") (Just $ toExtName "StdString") []
  [ mkCtor "new" []
  , mkCtor "newFromCString" [ptrT $ constT charT]
  , mkCtor "newFromCStringLen_raw" [ptrT $ constT charT, sizeT]
  , mkMethod' "at" "at" [intT] $ refT charT
  , mkConstMethod' "at" "get" [intT] charT
  , mkConstMethod "c_str" [] $ ptrT $ constT charT
  , mkConstMethod "data" [] $ ptrT $ constT charT
  , mkConstMethod "size" [] sizeT
  ]
  where
    addendum = do
      addImports $ mconcat [hsImportForPrelude, hsImportForForeignC]
      addExport "stdString_newFromCStringLen"
      sayLn "stdString_newFromCStringLen :: HoppyFC.CStringLen -> HoppyP.IO StdString"
      sayLn "stdString_newFromCStringLen (p,n) ="
      indent $ do
        sayLn "stdString_newFromCStringLen_raw p (HoppyP.fromIntegral n)"
