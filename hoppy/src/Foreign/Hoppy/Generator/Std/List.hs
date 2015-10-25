-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, BidirectionalIterator, Comparable, Copyable, Equatable),
  IteratorMutability (Constant, Mutable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)

-- | Options for instantiating the list classes.
data Options = Options
  { optListClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @std::list@ class.  Lists are always
    -- 'Assignable' and 'Copyable', but you may want to add 'Equatable' and
    -- 'Comparable' if your value type supports those.
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options []

-- | A set of instantiated list classes.
data Contents = Contents
  { c_list :: Class  -- ^ @std::list\<T>@
  , c_iterator :: Class  -- ^ @std::list\<T>::iterator@
  , c_constIterator :: Class  -- ^ @std::list\<T>::const_iterator@
  }

-- | @instantiate className t tReqs@ creates a set of bindings for an
-- instantiation of @std::list@ and associated types (e.g. iterators).  In the
-- result, the 'c_list' class has an external name of @\"list\" ++ className@,
-- and the iterator classes are further suffixed with @\"Iterator\"@ and
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
        addUseReqs reqs $
        classAddFeatures features $
        makeClass (ident1T "std" "list" [t]) (Just $ toExtName listName) []
        [ mkCtor "new" []
        ] $
        collect
        [ just $ mkMethod' "back" "back" [] $ TRef t
        , just $ mkConstMethod' "back" "backConst" [] $ TRef $ TConst t
        , just $ mkMethod' "begin" "begin" [] $ TObjToHeap iterator
        , just $ mkConstMethod' "begin" "beginConst" [] $ TObjToHeap constIterator
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
          [TObj iterator, TRef $ TConst t] $ TObjToHeap iterator
        , just $ mkConstMethod' "max_size" "maxSize" [] TSize
        , test (elem Comparable features) $ mkMethod "merge" [TRef $ TObj list] TVoid
          -- TODO merge(list&, Comparator)
        , just $ mkMethod "pop_back" [] TVoid
        , just $ mkMethod "pop_front" [] TVoid
        , just $ mkMethod "push_back" [TRef $ TConst t] TVoid
        , just $ mkMethod "push_front" [TRef $ TConst t] TVoid
        , test (elem Equatable features) $ mkMethod "remove" [TRef $ TConst t] TVoid
          -- TODO remove_if(UnaryPredicate)
        , just $ mkMethod' "resize" "resize" [TSize] TVoid
        , just $ mkMethod' "resize" "resizeWith" [TSize, t] TVoid
        , just $ mkMethod "reverse" [] TVoid
        , just $ mkConstMethod "size" [] TSize
        , test (elem Comparable features) $ mkMethod "sort" [] TVoid
          -- TODO sort(Comparator)
        , just $ mkMethod' "splice" "spliceAll" [TObj iterator, TRef $ TObj list] TVoid
        , just $ mkMethod' "splice" "spliceOne"
          [TObj iterator, TRef $ TObj list, TObj iterator] TVoid
        , just $ mkMethod' "splice" "spliceRange"
          [TObj iterator, TRef $ TObj list, TObj iterator, TObj iterator] TVoid
        , just $ mkMethod "swap" [TRef $ TObj list] TVoid
        , test (Equatable `elem` features) $ mkMethod "unique" [] TVoid
          -- TODO unique(BinaryPredicate)
        ]

      iterator =
        addUseReqs reqs $
        classAddFeatures [BidirectionalIterator Mutable $ Just t] $
        makeClass (identT' [("std", Nothing), ("list", Just [t]), ("iterator", Nothing)])
        (Just $ toExtName iteratorName) [] [] []

      constIterator =
        addUseReqs reqs $
        classAddFeatures [BidirectionalIterator Constant $ Just t] $
        makeClass (identT' [("std", Nothing), ("list", Just [t]), ("const_iterator", Nothing)])
        (Just $ toExtName constIteratorName)
        []
        [ mkCtor "newFromConst" [TObj iterator]
        ]
        [ makeFnMethod (ident2 "hoppy" "iterator" "deconst") "deconst" MConst Nonpure
          [TRef $ TConst $ TObj constIterator, TRef $ TObj list] $ TObjToHeap iterator
        ]

  in Contents
     { c_list = list
     , c_iterator = iterator
     , c_constIterator = constIterator
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [Export]
toExports m = map (ExportClass . ($ m)) [c_list, c_iterator, c_constIterator]
