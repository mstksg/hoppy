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

-- | Bindings for @std::map@.
module Foreign.Hoppy.Generator.Std.Map (
  Options (..),
  defaultOptions,
  Contents (..),
  instantiate,
  instantiate',
  toExports,
  ) where

import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, BidirectionalIterator, Copyable),
  IteratorMutability (Constant, Mutable),
  classAddFeatures,
  )

-- | Options for instantiating the map classes.
data Options = Options
  { optMapClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @std::map@ class.  Maps are always
    -- 'Assignable' and 'Copyable'.
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options []

-- | A set of instantiated map classes.
data Contents = Contents
  { c_map :: Class  -- ^ @std::map\<K, V>@
  , c_iterator :: Class  -- ^ @std::map\<K, V>::iterator@
  , c_constIterator :: Class  -- ^ @std::map\<K, V>::const_iterator@
  }

-- | @instantiate className k v@ creates a set of bindings for an instantiation
-- of @std::map\<k, v\>@ and associated types (e.g. iterators).  In the result,
-- the 'c_map' class has an external name of @className@, and the iterator
-- classes are further suffixed with @\"Iterator\"@ and @\"ConstIterator\"@
-- respectively.
instantiate :: String -> Type -> Type -> Contents
instantiate mapName k v = instantiate' mapName k v defaultOptions

-- | 'instantiate' with additional options.
instantiate' :: String -> Type -> Type -> Options -> Contents
instantiate' mapName k v opts =
  let reqs = mconcat
             [ reqInclude $ includeStd "hoppy/map.hpp"
             , reqInclude $ includeStd "map"
             ]
      iteratorName = mapName ++ "Iterator"
      constIteratorName = mapName ++ "ConstIterator"

      getIteratorKeyIdent = ident2T "hoppy" "map" "getIteratorKey" [k, v]
      getIteratorValueIdent = ident2T "hoppy" "map" "getIteratorValue" [k, v]

      map =
        addUseReqs reqs $
        classAddFeatures (Assignable : Copyable : optMapClassFeatures opts) $
        makeClass (ident1T "std" "map" [k, v]) (Just $ toExtName mapName) []
        [ mkCtor "new" []
        ]
        [ mkMethod' "at" "at" [k] $ TRef v
        , mkConstMethod' "at" "atConst" [k] $ TRef $ TConst v
        , mkMethod' "begin" "begin" [] $ TObjToHeap iterator
        , mkConstMethod' "begin" "beginConst" [] $ TObjToHeap constIterator
        , mkMethod "clear" [] TVoid
        , mkConstMethod "count" [k] TSize
        , mkConstMethod "empty" [] TBool
        , mkMethod' "end" "end" [] $ TObjToHeap iterator
        , mkConstMethod' "end" "endConst" [] $ TObjToHeap constIterator
          -- equal_range: find is good enough.
        , mkMethod' "erase" "erase" [TObj iterator] TVoid
        , mkMethod' "erase" "eraseKey" [k] TSize
        , mkMethod' "erase" "eraseRange" [TObj iterator, TObj iterator] TVoid
        , mkMethod' "find" "find" [k] $ TObjToHeap iterator
        , mkConstMethod' "find" "findConst" [k] $ TObjToHeap constIterator
          -- TODO insert
          -- lower_bound: find is good enough.
        , mkConstMethod' "max_size" "maxSize" [] TSize
        , mkConstMethod "size" [] TSize
        , mkMethod "swap" [TRef $ TObj map] TVoid
          -- upper_bound: find is good enough.
        , mkMethod OpArray [k] $ TRef v
        ]

      iterator =
        addUseReqs reqs $
        classAddFeatures [BidirectionalIterator Mutable Nothing] $
        makeClass (identT' [("std", Nothing),
                            ("map", Just [k, v]),
                            ("iterator", Nothing)])
        (Just $ toExtName iteratorName) [] []
        [ makeFnMethod getIteratorKeyIdent "getKey" MConst Nonpure
          [TRef $ TConst $ TObj iterator] $ TRef $ TConst k
        , makeFnMethod getIteratorValueIdent "getValue" MNormal Nonpure
          [TRef $ TObj iterator] $ TRef v
        , makeFnMethod getIteratorValueIdent "getValueConst" MConst Nonpure
          [TRef $ TConst $ TObj iterator] $ TRef $ TConst v
        ]

      constIterator =
        addUseReqs reqs $
        classAddFeatures [BidirectionalIterator Constant Nothing] $
        makeClass (identT' [("std", Nothing),
                            ("map", Just [k, v]),
                            ("const_iterator", Nothing)])
        (Just $ toExtName constIteratorName)
        []
        [ mkCtor "newFromConst" [TObj iterator]
        ]
        [ makeFnMethod (ident2 "hoppy" "iterator" "deconst") "deconst" MConst Nonpure
          [TRef $ TConst $ TObj constIterator, TRef $ TObj map] $ TObjToHeap iterator
        , makeFnMethod getIteratorKeyIdent "getKey" MConst Nonpure
          [TRef $ TConst $ TObj constIterator] $ TRef $ TConst k
        , makeFnMethod getIteratorValueIdent "getValue" MConst Nonpure
          [TRef $ TConst $ TObj constIterator] $ TRef $ TConst v
        ]

  in Contents
     { c_map = map
     , c_iterator = iterator
     , c_constIterator = constIterator
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [Export]
toExports m = map (ExportClass . ($ m)) [c_map, c_iterator, c_constIterator]
