-- This file is part of Hoppy.
--
-- Copyright 2015-2023 Bryan Gardiner <bog@khumba.net>
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

-- | Bindings for @std::vector@.
module Foreign.Hoppy.Generator.Std.Vector (
  Options (..),
  defaultOptions,
  Contents (..),
  instantiate,
  instantiate',
  toExports,
  ) where

import Control.Monad (forM_, when)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell (
  HsTypeSide (HsHsSide),
  addImports,
  cppTypeToHsTypeAndUse,
  indent,
  ln,
  prettyPrint,
  sayLn,
  saysLn,
  )
import Foreign.Hoppy.Generator.Spec (
  Constness (Const, Nonconst),
  Export (Export),
  Purity (Nonpure),
  Reqs,
  Type,
  addAddendumHaskell,
  addReqs,
  hsImport1,
  hsImports,
  hsImportForPrelude,
  hsImportForRuntime,
  ident1T,
  ident2,
  identT',
  includeStd,
  np,
  reqInclude,
  toExtName,
  )
import Foreign.Hoppy.Generator.Spec.Class (
  Class,
  MethodApplicability (MConst),
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  toHsDataTypeName,
  toHsClassEntityName,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Std (ValueConversion (ConvertPtr, ConvertValue))
import Foreign.Hoppy.Generator.Std.Iterator
import Foreign.Hoppy.Generator.Types
import Foreign.Hoppy.Generator.Version (CppVersion (Cpp2011), activeCppVersion, collect, just, test)

-- | Options for instantiating the vector classes.
data Options = Options
  { optVectorClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @std::vector@ class.  Vectors are
    -- always 'Assignable' and 'Copyable', but you may want to add
    -- 'Foreign.Hoppy.Generator.Spec.ClassFeature.Equatable' and
    -- 'Foreign.Hoppy.Generator.Spec.ClassFeature.Comparable' if your value type
    -- supports those.
  , optValueConversion :: Maybe ValueConversion
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options [] Nothing

-- | A set of instantiated vector classes.
data Contents = Contents
  { c_vector :: Class  -- ^ @std::vector\<T>@
  , c_iterator :: Class  -- ^ @std::vector\<T>::iterator@
  , c_constIterator :: Class  -- ^ @std::vector\<T>::const_iterator@
  }

-- | @instantiate className t tReqs@ creates a set of bindings for an
-- instantiation of @std::vector@ and associated types (e.g. iterators).  In the
-- result, the 'c_vector' class has an external name of @className@, and the
-- iterator classes are further suffixed with @\"Iterator\"@ and
-- @\"ConstIterator\"@ respectively.
instantiate :: String -> Type -> Reqs -> Contents
instantiate vectorName t tReqs = instantiate' vectorName t tReqs defaultOptions

-- | 'instantiate' with additional options.
instantiate' :: String -> Type -> Reqs -> Options -> Contents
instantiate' vectorName t tReqs opts =
  let reqs = mconcat [tReqs, reqInclude $ includeStd "vector"]
      iteratorName = vectorName ++ "Iterator"
      constIteratorName = vectorName ++ "ConstIterator"

      vector =
        (case optValueConversion opts of
           Nothing -> id
           Just conversion -> addAddendumHaskell $ makeAddendum conversion) $
        addReqs reqs $
        classAddFeatures (Assignable : Copyable : optVectorClassFeatures opts) $
        makeClass (ident1T "std" "vector" [t]) (Just $ toExtName vectorName) [] $
        collect
        [ just $ mkCtor "new" np
        , just $ mkMethod' "at" "at" [sizeT] $ refT t
        , just $ mkConstMethod' "at" "atConst" [sizeT] $ refT $ constT t
        , just $ mkMethod' "back" "back" np $ refT t
        , just $ mkConstMethod' "back" "backConst" np $ refT $ constT t
        , just $ mkMethod' "begin" "begin" np $ toGcT $ objT iterator
        , just $ mkConstMethod' "begin" "beginConst" np $ toGcT $ objT constIterator
        , just $ mkConstMethod "capacity" np sizeT
        , just $ mkMethod "clear" np voidT
        , just $ mkConstMethod "empty" np boolT
        , just $ mkMethod' "end" "end" np $ toGcT $ objT iterator
        , just $ mkConstMethod' "end" "endConst" np $ toGcT $ objT constIterator
        , just $ mkMethod' "erase" "erase" [objT iterator] voidT
        , just $ mkMethod' "erase" "eraseRange" [objT iterator, objT iterator] voidT
        , just $ mkMethod' "front" "front" np $ refT t
        , just $ mkConstMethod' "front" "frontConst" np $ refT $ constT t
        , just $ mkMethod "insert" [objT iterator, t] $ toGcT $ objT iterator
        , just $ mkConstMethod' "max_size" "maxSize" np sizeT
        , just $ mkMethod' "pop_back" "popBack" np voidT
        , just $ mkMethod' "push_back" "pushBack" [t] voidT
        , just $ mkMethod "reserve" [sizeT] voidT
        , just $ mkMethod' "resize" "resize" [sizeT] voidT
        , just $ mkMethod' "resize" "resizeWith" [sizeT, t] voidT
        , test (activeCppVersion >= Cpp2011) $ mkMethod' "shrink_to_fit" "shrinkToFit" np voidT
        , just $ mkConstMethod "size" np sizeT
        , just $ mkMethod "swap" [refT $ objT vector] voidT
        ]

      iterator =
        addReqs reqs $
        makeRandomIterator Mutable (Just t) ptrdiffT $
        makeClass (identT' [("std", Nothing), ("vector", Just [t]), ("iterator", Nothing)])
        (Just $ toExtName iteratorName) [] []

      constIterator =
        addReqs reqs $
        makeRandomIterator Constant (Just t) ptrdiffT $
        makeClass (identT' [("std", Nothing), ("vector", Just [t]), ("const_iterator", Nothing)])
        (Just $ toExtName constIteratorName) []
        [ mkCtor "newFromNonconst" [objT iterator]
        , makeFnMethod (ident2 "hoppy" "iterator" "deconst") "deconst" MConst Nonpure
          [objT constIterator, refT $ objT vector] $ toGcT $ objT iterator
        ]

      -- The addendum for the vector class contains HasContents and FromContents
      -- instances.
      makeAddendum conversion = do
        addImports $ mconcat [hsImports "Prelude" ["($)", "(-)"],
                              hsImportForPrelude,
                              hsImportForRuntime]
        when (conversion == ConvertValue) $
          addImports $ hsImport1 "Control.Monad" "(<=<)"

        forM_ [Const, Nonconst] $ \cst -> do
          hsDataTypeName <- toHsDataTypeName cst vector
          hsValueType <-
            cppTypeToHsTypeAndUse HsHsSide $
            (case conversion of
               ConvertPtr -> ptrT
               ConvertValue -> id) $
            case cst of
              Const -> constT t
              Nonconst -> t

          -- Generate const and nonconst HasContents instances.
          ln
          saysLn ["instance HoppyFHR.HasContents ", hsDataTypeName,
                  " (", prettyPrint hsValueType, ") where"]
          indent $ do
            sayLn "toContents this' = do"
            indent $ do
              vectorAt <- toHsClassEntityName vector $ case cst of
                Const -> "atConst"
                Nonconst -> "at"
              vectorSize <- toHsClassEntityName vector "size"

              saysLn ["size' <- ", vectorSize, " this'"]
              saysLn ["HoppyP.mapM (",
                      case conversion of
                        ConvertPtr -> ""
                        ConvertValue -> "HoppyFHR.decode <=< ",
                      vectorAt, " this') [0..size'-1]"]

          -- Only generate a nonconst FromContents instance.
          when (cst == Nonconst) $ do
            ln
            saysLn ["instance HoppyFHR.FromContents ", hsDataTypeName,
                    " (", prettyPrint hsValueType, ") where"]
            indent $ do
              sayLn "fromContents values' = do"
              indent $ do
                vectorNew <- toHsClassEntityName vector "new"
                vectorPushBack <- toHsClassEntityName vector "pushBack"
                vectorReserve <- toHsClassEntityName vector "reserve"
                saysLn ["vector' <- ", vectorNew]
                saysLn [vectorReserve, " vector' $ HoppyFHR.coerceIntegral $ HoppyP.length values'"]
                saysLn ["HoppyP.mapM_ (", vectorPushBack, " vector') values'"]
                sayLn "HoppyP.return vector'"

  in Contents
     { c_vector = vector
     , c_iterator = iterator
     , c_constIterator = constIterator
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [Export]
toExports m = map (Export . ($ m)) [c_vector, c_iterator, c_constIterator]
