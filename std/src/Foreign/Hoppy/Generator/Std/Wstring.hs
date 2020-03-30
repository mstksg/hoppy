-- This file is part of Hoppy.
--
-- Copyright 2015-2020 Bryan Gardiner <bog@khumba.net>
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

-- | Bindings for @std::wstring@.
module Foreign.Hoppy.Generator.Std.Wstring (c_wstring) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell (addExport, addImports, indent, sayLn)
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.Class (
  Class,
  ClassHaskellConversion (..),
  classSetHaskellConversion,
  makeClass,
  mkCtor,
  mkConstMethod,
  mkConstMethod',
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

-- | @std::wstring@
c_wstring :: Class
c_wstring =
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
        sayLn "HoppyP.flip HoppyFC.withCWStringLen stdWstring_newFromCWStringLen"
      , classHaskellConversionFromCppFn = Just $ do
        addImports $ mconcat [hsImport1 "Control.Applicative" "(<*)",
                              hsImportForForeignC,
                              hsImportForPrelude,
                              hsImportForRuntime]
        sayLn "\\s -> do"
        indent $ do
          sayLn "p <- stdWstring_data s"
          sayLn "n <- stdWstring_size s"
          sayLn "HoppyFC.peekCWStringLen (p, HoppyP.fromIntegral n) <* HoppyFHR.touchCppPtr s"
      } $
  makeClass (ident1 "std" "wstring") (Just $ toExtName "StdWstring") []
  [ mkCtor "new" np
  , mkCtor "newFromCWString" [ptrT $ constT wcharT]
  , mkCtor "newFromCWStringLen_raw" [ptrT $ constT wcharT, sizeT]
  , mkMethod' "at" "at" [intT] $ refT wcharT
  , mkConstMethod' "at" "get" [intT] wcharT
  , mkConstMethod "c_str" np $ ptrT $ constT wcharT
  , mkConstMethod "data" np $ ptrT $ constT wcharT
  , mkConstMethod "size" np sizeT
  ]
  where
    addendum = do
      addImports $ mconcat [hsImportForPrelude, hsImportForForeignC]
      addExport "stdWstring_newFromCWStringLen"
      sayLn "stdWstring_newFromCWStringLen :: HoppyFC.CWStringLen -> HoppyP.IO StdWstring"
      sayLn "stdWstring_newFromCWStringLen (p,n) ="
      indent $ sayLn "stdWstring_newFromCWStringLen_raw p (HoppyP.fromIntegral n)"
