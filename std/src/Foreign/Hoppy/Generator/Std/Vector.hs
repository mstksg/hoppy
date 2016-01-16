-- This file is part of Hoppy.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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
  toHsDataTypeName,
  toHsMethodName',
  )
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Std (ValueConversion (ConvertPtr, ConvertValue))
import Foreign.Hoppy.Generator.Std.Iterator
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
        makeClass (ident1T "std" "vector" [t]) (Just $ toExtName vectorName) []
        [ mkCtor "new" []
        ] $
        collect
        [ just $ mkMethod' "at" "at" [TSize] $ TRef t
        , just $ mkConstMethod' "at" "atConst" [TSize] $ TRef $ TConst t
        , just $ mkMethod' "back" "back" [] $ TRef t
        , just $ mkConstMethod' "back" "backConst" [] $ TRef $ TConst t
        , just $ mkMethod' "begin" "begin" [] $ TObjToHeap iterator
        , just $ mkConstMethod' "begin" "beginConst" [] $ TObjToHeap constIterator
        , just $ mkConstMethod "capacity" [] TSize
        , just $ mkMethod "clear" [] TVoid
        , just $ mkConstMethod "empty" [] TBool
        , just $ mkMethod' "end" "end" [] $ TObjToHeap iterator
        , just $ mkConstMethod' "end" "endConst" [] $ TObjToHeap constIterator
        , just $ mkMethod' "erase" "erase" [TObj iterator] TVoid
        , just $ mkMethod' "erase" "eraseRange" [TObj iterator, TObj iterator] TVoid
        , just $ mkMethod' "front" "front" [] $ TRef t
        , just $ mkConstMethod' "front" "frontConst" [] $ TRef $ TConst t
        , just $ mkMethod' "insert" "insert" [TObj iterator, TRef $ TConst t] TVoid
        , just $ mkMethod' "insert" "insertAndGetIterator"
          [TObj iterator, t] $ TObjToHeap iterator
        , just $ mkConstMethod' "max_size" "maxSize" [] TSize
        , just $ mkMethod' "pop_back" "popBack" [] TVoid
        , just $ mkMethod' "push_back" "pushBack" [t] TVoid
        , just $ mkMethod "reserve" [TSize] TVoid
        , just $ mkMethod' "resize" "resize" [TSize] TVoid
        , just $ mkMethod' "resize" "resizeWith" [TSize, t] TVoid
        , test (activeCppVersion >= Cpp2011) $ mkMethod' "shrink_to_fit" "shrinkToFit" [] TVoid
        , just $ mkConstMethod "size" [] TSize
        , just $ mkMethod "swap" [TRef $ TObj vector] TVoid
        ]

      iterator =
        addReqs reqs $
        makeRandomIterator Mutable (Just t) TPtrdiff $
        makeClass (identT' [("std", Nothing), ("vector", Just [t]), ("iterator", Nothing)])
        (Just $ toExtName iteratorName) [] [] []

      constIterator =
        addReqs reqs $
        makeRandomIterator Constant (Just t) TPtrdiff $
        makeClass (identT' [("std", Nothing), ("vector", Just [t]), ("const_iterator", Nothing)])
        (Just $ toExtName constIteratorName) []
        [ mkCtor "newFromNonconst" [TObj iterator]
        ]
        [ makeFnMethod (ident2 "hoppy" "iterator" "deconst") "deconst" MConst Nonpure
          [TObj constIterator, TRef $ TObj vector] $ TObjToHeap iterator
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
          let hsDataTypeName = toHsDataTypeName cst vector
          hsValueType <-
            cppTypeToHsTypeAndUse HsHsSide $
            (case conversion of
               ConvertPtr -> TPtr
               ConvertValue -> id) $
            case cst of
              Const -> TConst t
              Nonconst -> t

          -- Generate const and nonconst HasContents instances.
          ln
          saysLn ["instance HoppyFHR.HasContents ", hsDataTypeName,
                  " (", prettyPrint hsValueType, ") where"]
          indent $ do
            sayLn "toContents this' = do"
            indent $ do
              let vectorAt = case cst of
                    Const -> "atConst"
                    Nonconst -> "at"
              saysLn ["size' <- ", toHsMethodName' vector "size", " this'"]
              saysLn ["HoppyP.mapM (",
                      case conversion of
                        ConvertPtr -> ""
                        ConvertValue -> "HoppyFHR.decode <=< ",
                      toHsMethodName' vector vectorAt, " this') [0..size'-1]"]

          -- Only generate a nonconst FromContents instance.
          when (cst == Nonconst) $ do
            ln
            saysLn ["instance HoppyFHR.FromContents ", hsDataTypeName,
                    " (", prettyPrint hsValueType, ") where"]
            indent $ do
              sayLn "fromContents values' = do"
              indent $ do
                saysLn ["vector' <- ", toHsMethodName' vector "new"]
                saysLn [toHsMethodName' vector "reserve",
                        " vector' $ HoppyFHR.coerceIntegral $ HoppyP.length values'"]
                saysLn ["HoppyP.mapM_ (", toHsMethodName' vector "pushBack", " vector') values'"]
                sayLn "HoppyP.return vector'"

  in Contents
     { c_vector = vector
     , c_iterator = iterator
     , c_constIterator = constIterator
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [Export]
toExports m = map (ExportClass . ($ m)) [c_vector, c_iterator, c_constIterator]
