-- This file is part of Hoppy.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

-- | Bindings for common class operations, such as copy construction.
module Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (..),
  classAddFeatures,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Types

-- | Sets of functionality that can be stamped onto a class with
-- 'classAddFeatures'.
data ClassFeature =
    Assignable
    -- ^ Provides the assignment operator, @Foo& Foo::operator=(const Foo&)@.
  | Comparable
    -- ^ Provides operators @<@, @<=@, @>@, @>=@, for example @bool
    -- Foo::operator<(const Foo&)@.  This feature does not automatically include
    -- 'Equatable'.
  | Copyable
    -- ^ Provides copy construction, @Foo::Foo(const Foo&)@.
  | Equatable
    -- ^ Provides @operator==@ and @operator!=@, for example @bool
    -- Foo::operator==(const Foo&)@.
  deriving (Eq, Show)

featureContents :: ClassFeature -> Class -> ([Ctor], [Method], Reqs)
featureContents feature cls = case feature of
  Assignable -> assignableContents cls
  Comparable -> comparableContents cls
  Copyable -> copyableContents cls
  Equatable -> equatableContents cls

assignableContents :: Class -> ([Ctor], [Method], Reqs)
assignableContents cls =
  ([],
   [ mkMethod OpAssign [refT $ constT $ objT cls] $ refT $ objT cls
   ],
   mempty)

comparableContents :: Class -> ([Ctor], [Method], Reqs)
comparableContents cls =
  ([],
   [ mkConstMethod OpLt [refT $ constT $ objT cls] boolT
   , mkConstMethod OpLe [refT $ constT $ objT cls] boolT
   , mkConstMethod OpGt [refT $ constT $ objT cls] boolT
   , mkConstMethod OpGe [refT $ constT $ objT cls] boolT
   ],
   mempty)

copyableContents :: Class -> ([Ctor], [Method], Reqs)
copyableContents cls =
  ([ mkCtor "newCopy" [objT cls]
   ],
   [],
   mempty)

equatableContents :: Class -> ([Ctor], [Method], Reqs)
equatableContents cls =
  ([],
   [ mkConstMethod OpEq [objT cls] boolT
   , mkConstMethod OpNe [objT cls] boolT
   ],
   mempty)

-- | Adds the contents of a feature to a class.  Does not check for overlap with
-- existing class contents.
classAddFeatures :: [ClassFeature] -> Class -> Class
classAddFeatures features cls =
  foldr (\feature cls' ->
          let (ctors, methods, reqs) = featureContents feature cls'
          in addReqs reqs $
             classAddCtors ctors $
             classAddMethods methods cls')
        cls
        features
