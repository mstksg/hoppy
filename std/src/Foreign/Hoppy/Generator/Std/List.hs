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

-- | Bindings for @std::list@.
module Foreign.Hoppy.Generator.Std.List (
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
  toHsClassEntityName,
  )
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Std (ValueConversion (ConvertPtr, ConvertValue))
import Foreign.Hoppy.Generator.Std.Iterator
import Foreign.Hoppy.Generator.Types
import Foreign.Hoppy.Generator.Version (collect, just, test)

-- | Options for instantiating the list classes.
data Options = Options
  { optListClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @std::list@ class.  Lists are always
    -- 'Assignable' and 'Copyable', but you may want to add 'Equatable' and
    -- 'Comparable' if your value type supports those.
  , optValueConversion :: Maybe ValueConversion
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options [] Nothing

-- | A set of instantiated list classes.
data Contents = Contents
  { c_list :: Class  -- ^ @std::list\<T>@
  , c_iterator :: Class  -- ^ @std::list\<T>::iterator@
  , c_constIterator :: Class  -- ^ @std::list\<T>::const_iterator@
  }

-- | @instantiate className t tReqs@ creates a set of bindings for an
-- instantiation of @std::list@ and associated types (e.g. iterators).  In the
-- result, the 'c_list' class has an external name of @className@, and the
-- iterator classes are further suffixed with @\"Iterator\"@ and
-- @\"ConstIterator\"@ respectively.
instantiate :: String -> Type -> Reqs -> Contents
instantiate listName t tReqs = instantiate' listName t tReqs defaultOptions

-- | 'instantiate' with additional options.
instantiate' :: String -> Type -> Reqs -> Options -> Contents
instantiate' listName t tReqs opts =
  let reqs = mconcat [tReqs, reqInclude $ includeStd "list"]
      iteratorName = listName ++ "Iterator"
      constIteratorName = listName ++ "ConstIterator"
      features = Assignable : Copyable : optListClassFeatures opts

      list =
        (case optValueConversion opts of
           Nothing -> id
           Just conversion -> addAddendumHaskell $ makeAddendum conversion) $
        addReqs reqs $
        classAddFeatures features $
        makeClass (ident1T "std" "list" [t]) (Just $ toExtName listName) [] $
        collect
        [ just $ mkCtor "new" []
        , just $ mkMethod' "back" "back" [] $ refT t
        , just $ mkConstMethod' "back" "backConst" [] $ refT $ constT t
        , just $ mkMethod' "begin" "begin" [] $ toGcT $ objT iterator
        , just $ mkConstMethod' "begin" "beginConst" [] $ toGcT $ objT constIterator
        , just $ mkMethod "clear" [] voidT
        , just $ mkConstMethod "empty" [] boolT
        , just $ mkMethod' "end" "end" [] $ toGcT $ objT iterator
        , just $ mkConstMethod' "end" "endConst" [] $ toGcT $ objT constIterator
        , just $ mkMethod' "erase" "erase" [objT iterator] voidT
        , just $ mkMethod' "erase" "eraseRange" [objT iterator, objT iterator] voidT
        , just $ mkMethod' "front" "front" [] $ refT t
        , just $ mkConstMethod' "front" "frontConst" [] $ refT $ constT t
        , just $ mkMethod "insert" [objT iterator, t] $ toGcT $ objT iterator
        , just $ mkConstMethod' "max_size" "maxSize" [] sizeT
        , test (elem Comparable features) $ mkMethod "merge" [refT $ objT list] voidT
          -- TODO merge(list&, Comparator)
        , just $ mkMethod' "pop_back" "popBack" [] voidT
        , just $ mkMethod' "pop_front" "popFront" [] voidT
        , just $ mkMethod' "push_back" "pushBack" [t] voidT
        , just $ mkMethod' "push_front" "pushFront" [t] voidT
        , test (elem Equatable features) $ mkMethod "remove" [t] voidT
          -- TODO remove_if(UnaryPredicate)
        , just $ mkMethod' "resize" "resize" [sizeT] voidT
        , just $ mkMethod' "resize" "resizeWith" [sizeT, t] voidT
        , just $ mkMethod "reverse" [] voidT
        , just $ mkConstMethod "size" [] sizeT
        , test (elem Comparable features) $ mkMethod "sort" [] voidT
          -- TODO sort(Comparator)
        , just $ mkMethod' "splice" "spliceAll" [objT iterator, refT $ objT list] voidT
        , just $ mkMethod' "splice" "spliceOne"
          [objT iterator, refT $ objT list, objT iterator] voidT
        , just $ mkMethod' "splice" "spliceRange"
          [objT iterator, refT $ objT list, objT iterator, objT iterator] voidT
        , just $ mkMethod "swap" [refT $ objT list] voidT
        , test (Equatable `elem` features) $ mkMethod "unique" [] voidT
          -- TODO unique(BinaryPredicate)
        ]

      iterator =
        addReqs reqs $
        makeBidirectionalIterator Mutable (Just t) $
        makeClass (identT' [("std", Nothing), ("list", Just [t]), ("iterator", Nothing)])
        (Just $ toExtName iteratorName) [] []

      constIterator =
        addReqs reqs $
        makeBidirectionalIterator Constant (Just t) $
        makeClass (identT' [("std", Nothing), ("list", Just [t]), ("const_iterator", Nothing)])
        (Just $ toExtName constIteratorName)
        []
        [ mkCtor "newFromConst" [objT iterator]
        , makeFnMethod (ident2 "hoppy" "iterator" "deconst") "deconst" MConst Nonpure
          [objT constIterator, refT $ objT list] $ toGcT $ objT iterator
        ]

      -- The addendum for the list class contains HasContents and FromContents
      -- instances.
      makeAddendum conversion = do
        addImports $ mconcat [hsImport1 "Prelude" "($)",
                              hsImportForPrelude,
                                hsImportForRuntime]
        when (conversion == ConvertValue) $
          addImports $ hsImport1 "Prelude" "(=<<)"

        forM_ [Const, Nonconst] $ \cst -> do
          hsDataTypeName <- toHsDataTypeName cst list
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
              listEmpty <- toHsClassEntityName list "empty"
              listBegin <- toHsClassEntityName list $ case cst of
                Const -> "beginConst"
                Nonconst -> "begin"
              listEnd <- toHsClassEntityName list $ case cst of
                Const -> "endConst"
                Nonconst -> "end"
              let iter = case cst of
                    Const -> constIterator
                    Nonconst -> iterator
              iterEq <- toHsClassEntityName iter OpEq
              iterGet <- toHsClassEntityName iter $ case cst of
                Const -> "getConst"
                Nonconst -> "get"
              iterPrev <- toHsClassEntityName iter "prev"

              saysLn ["empty' <- ", listEmpty, " this'"]
              sayLn "if empty' then HoppyP.return [] else do"
              indent $ do
                saysLn ["begin' <- ", listBegin, " this'"]
                saysLn ["iter' <- ", listEnd, " this'"]
                sayLn "go' iter' begin' []"
              sayLn "where"
              indent $ do
                sayLn "go' iter' begin' acc' = do"
                indent $ do
                  saysLn ["stop' <- ", iterEq, " iter' begin'"]
                  sayLn "if stop' then HoppyP.return acc' else do"
                  indent $ do
                    saysLn ["_ <- ", iterPrev, " iter'"]
                    saysLn ["value' <- ",
                            case conversion of
                              ConvertPtr -> ""
                              ConvertValue -> "HoppyFHR.decode =<< ",
                            iterGet, " iter'"]
                    sayLn "go' iter' begin' $ value':acc'"

          -- Only generate a nonconst FromContents instance.
          when (cst == Nonconst) $ do
            ln
            saysLn ["instance HoppyFHR.FromContents ", hsDataTypeName,
                    " (", prettyPrint hsValueType, ") where"]
            indent $ do
              sayLn "fromContents values' = do"
              indent $ do
                listNew <- toHsClassEntityName list "new"
                listPushBack <- toHsClassEntityName list "pushBack"
                saysLn ["list' <- ", listNew]
                saysLn ["HoppyP.mapM_ (", listPushBack, " list') values'"]
                sayLn "HoppyP.return list'"

  in Contents
     { c_list = list
     , c_iterator = iterator
     , c_constIterator = constIterator
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [Export]
toExports m = map (ExportClass . ($ m)) [c_list, c_iterator, c_constIterator]
