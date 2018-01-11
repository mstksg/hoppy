-- This file is part of Hoppy.
--
-- Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
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

{-# LANGUAGE CPP #-}

-- | The primary data types for specifying C++ interfaces.
--
-- 'Show' instances in this module produce strings of the form @\"\<TypeOfObject
-- nameOfObject otherInfo...\>\"@.  They can be used in error messages without
-- specifying a noun separately, i.e. write @show cls@ instead of @\"the class
-- \" ++ show cls@.
module Foreign.Hoppy.Generator.Spec.Conversion (
  -- * Advanced class conversions
  classSetConversionToHeap,
  classSetConversionToGc,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell
import Foreign.Hoppy.Generator.Spec.Base
import Foreign.Hoppy.Generator.Types

-- | Modifies a class's 'ClassConversion' structure by setting all languages
-- to use 'ClassConversionToHeap'.
classSetConversionToHeap :: Class -> Class
classSetConversionToHeap cls = case classFindCopyCtor cls of
  Just _ ->
    flip classModifyConversion cls $ \c ->
    c { classHaskellConversion = classHaskellConversionToHeap cls
      }
  Nothing -> error $ "classSetConversionToHeap: " ++ show cls ++ " must be copyable."

-- | Modifies a class's 'ClassConversion' structure by setting all languages
-- that support garbage collection to use 'ClassConversionToGc'.
classSetConversionToGc :: Class -> Class
classSetConversionToGc cls = case classFindCopyCtor cls of
  Just _ ->
    flip classModifyConversion cls $ \c ->
    c { classHaskellConversion = classHaskellConversionToGc cls
      }
  Nothing -> error $ "classSetConversionToGc: " ++ show cls ++ " must be copyable."

classHaskellConversionToHeap :: Class -> ClassHaskellConversion
classHaskellConversionToHeap cls =
  ClassHaskellConversion
  { classHaskellConversionType = Just $ cppTypeToHsTypeAndUse HsHsSide $ ptrT $ objT cls
  , classHaskellConversionToCppFn = Nothing
  , classHaskellConversionFromCppFn = Just $ do
      addImports hsImportForRuntime
      sayLn "HoppyFHR.copy"
  }

classHaskellConversionToGc :: Class -> ClassHaskellConversion
classHaskellConversionToGc cls =
  ClassHaskellConversion
  { classHaskellConversionType = Just $ cppTypeToHsTypeAndUse HsHsSide $ ptrT $ objT cls
  , classHaskellConversionToCppFn = Nothing
  , classHaskellConversionFromCppFn = Just $ do
      addImports $ mconcat [hsImport1 "Control.Monad" "(>=>)", hsImportForRuntime]
      sayLn "HoppyFHR.copy >=> HoppyFHR.toGc"
  }
