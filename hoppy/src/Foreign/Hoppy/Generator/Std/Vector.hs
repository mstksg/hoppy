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

-- | Bindings for @std::vector@.
module Foreign.Hoppy.Generator.Std.Vector (
  Options (..),
  defaultOptions,
  Contents (..),
  instantiate,
  instantiate',
  toExports,
  ) where

import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, RandomIterator),
  IteratorMutability (Constant, Mutable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Version (CppVersion (Cpp2011), activeCppVersion, collect, just, test)

-- | Options for instantiating the vector classes.
data Options = Options
  { optVectorClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @std::vector@ class.  Vectors are
    -- always 'Assignable' and 'Copyable', but you may want to add
    -- 'Foreign.Hoppy.Generator.Spec.ClassFeature.Equatable' and
    -- 'Foreign.Hoppy.Generator.Spec.ClassFeature.Comparable' if your value type
    -- supports those.
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options []

-- | A set of instantiated vector classes.
data Contents = Contents
  { c_vector :: Class  -- ^ @std::vector\<T\>@
  , c_iterator :: Class  -- ^ @std::vector\<T\>::iterator@
  , c_constIterator :: Class  -- ^ @std::vector\<T\>::const_iterator@
  }

-- | @instantiate classSuffix t@ creates a set of bindings for an instantiation
-- of @std::vector@ and associated types (e.g. iterators).  In the result, the
-- 'c_vector' class has an external name of @\"vector\" ++ classSuffix@, and the
-- iterator classes are further suffixed with @\"Iterator\"@ and
-- @\"ConstIterator\"@ respectively.
instantiate :: String -> Type -> Contents
instantiate classSuffix t = instantiate' classSuffix t defaultOptions

-- | 'instantiate' with additional options.
instantiate' :: String -> Type -> Options -> Contents
instantiate' classSuffix t opts =
  let reqs = reqInclude $ includeStd "vector"
      vectorName = "vector" ++ classSuffix
      iteratorName = vectorName ++ "Iterator"
      constIteratorName = vectorName ++ "ConstIterator"

      vector =
        addUseReqs reqs $
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
          [TObj iterator, TRef $ TConst t] $ TObjToHeap iterator
        , just $ mkConstMethod' "max_size" "maxSize" [] TSize
        , just $ mkMethod' "pop_back" "popBack" [] TVoid
        , just $ mkMethod' "push_back" "pushBack" [TRef $ TConst t] TVoid
        , just $ mkMethod "reserve" [TSize] TVoid
        , just $ mkMethod' "resize" "resize" [TSize] TVoid
        , just $ mkMethod' "resize" "resizeWith" [TSize, t] TVoid
        , test (activeCppVersion >= Cpp2011) $ mkMethod' "shrink_to_fit" "shrinkToFit" [] TVoid
        , just $ mkConstMethod "size" [] TSize
        , just $ mkMethod "swap" [TRef $ TObj vector] TVoid
        ]

      iterator =
        addUseReqs reqs $
        classAddFeatures [RandomIterator Mutable t TPtrdiff] $
        makeClass (identT' [("std", Nothing), ("vector", Just [t]), ("iterator", Nothing)])
        (Just $ toExtName iteratorName) [] [] []

      constIterator =
        addUseReqs reqs $
        classAddFeatures [RandomIterator Constant t TPtrdiff] $
        makeClass (identT' [("std", Nothing), ("vector", Just [t]), ("const_iterator", Nothing)])
        (Just $ toExtName constIteratorName) []
        [ mkCtor "newFromNonconst" [TObj iterator]
        ]
        [ makeFnMethod (ident2 "hoppy" "iterator" "deconst") "deconst" MConst Nonpure
          [TRef $ TConst $ TObj constIterator, TRef $ TObj vector] $ TObjToHeap iterator
        ]

  in Contents
     { c_vector = vector
     , c_iterator = iterator
     , c_constIterator = constIterator
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [Export]
toExports m = map (ExportClass . ($ m)) [c_vector, c_iterator, c_constIterator]
