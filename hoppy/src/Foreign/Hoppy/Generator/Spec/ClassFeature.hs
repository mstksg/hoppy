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

-- | Bindings for common class operations, such as copy construction.
module Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (..),
  IteratorMutability (..),
  classAddFeatures,
  ) where

import Data.Maybe (catMaybes)
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Version (collect, just)

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
  | TrivialIterator IteratorMutability (Maybe Type)
    -- ^ An STL trivial iterator.  Includes 'Assignable' and 'Copyable'.
    -- Provides: default construction; dereferencing if a type is given;
    -- assignment if mutable.
  | ForwardIterator IteratorMutability (Maybe Type)
    -- ^ An STL forward iterator.  Includes 'TrivialIterator' and provides
    -- pre-increment.
  | BidirectionalIterator IteratorMutability (Maybe Type)
    -- ^ An STL bidirectional iterator.  Includes 'ForwardIterator' and provides
    -- pre-decrement.
  | RandomIterator IteratorMutability (Maybe Type) Type
    -- ^ An STL random-access iterator.  Includes 'BidirectionalIterator' and
    -- provides arithmetic and array access.
  deriving (Eq, Show)

-- | Whether an iterator may be used to modify the underlying collection.
data IteratorMutability = Constant | Mutable
                        deriving (Eq, Ord, Show)

featureContents :: ClassFeature -> Class -> ([Ctor], [Method], Reqs)
featureContents feature cls = case feature of
  Assignable -> assignableContents cls
  Comparable -> comparableContents cls
  Copyable -> copyableContents cls
  Equatable -> equatableContents cls
  TrivialIterator mutable valueTypeMaybe -> trivialIteratorContents mutable cls valueTypeMaybe
  ForwardIterator mutable valueTypeMaybe -> forwardIteratorContents mutable cls valueTypeMaybe
  BidirectionalIterator mutable valueTypeMaybe ->
    bidirectionalIteratorContents mutable cls valueTypeMaybe
  RandomIterator mutable valueTypeMaybe distanceType ->
    randomIteratorContents mutable cls valueTypeMaybe distanceType

assignableContents :: Class -> ([Ctor], [Method], Reqs)
assignableContents cls =
  ([],
   [ mkMethod OpAssign [TRef $ TConst $ TObj cls] $ TRef $ TObj cls
   ],
   mempty)

comparableContents :: Class -> ([Ctor], [Method], Reqs)
comparableContents cls =
  ([],
   [ mkConstMethod OpLt [TRef $ TConst $ TObj cls] TBool
   , mkConstMethod OpLe [TRef $ TConst $ TObj cls] TBool
   , mkConstMethod OpGt [TRef $ TConst $ TObj cls] TBool
   , mkConstMethod OpGe [TRef $ TConst $ TObj cls] TBool
   ],
   mempty)

copyableContents :: Class -> ([Ctor], [Method], Reqs)
copyableContents cls =
  ([ mkCtor "newCopy" [TRef $ TConst $ TObj cls]
   ],
   [],
   mempty)

equatableContents :: Class -> ([Ctor], [Method], Reqs)
equatableContents cls =
  ([],
   [ mkConstMethod OpEq [TRef $ TConst $ TObj cls] TBool
   , mkConstMethod OpNe [TRef $ TConst $ TObj cls] TBool
   ],
   mempty)

trivialIteratorContents :: IteratorMutability -> Class -> Maybe Type -> ([Ctor], [Method], Reqs)
trivialIteratorContents mutable cls valueTypeMaybe =
  mconcat
  [ assignableContents cls
  , copyableContents cls
  , equatableContents cls
  , ([ mkCtor "new" []
     ],
     collect
     [ do valueType <- valueTypeMaybe
          Mutable <- Just mutable
          just $ mkMethod' OpDeref "get" [] $ TRef valueType
     , do valueType <- valueTypeMaybe
          return $ mkConstMethod' OpDeref "getConst" [] $ TRef $ TConst valueType
     , do valueType <- valueTypeMaybe
          Mutable <- Just mutable
          return $
            makeFnMethod (ident2 "hoppy" "iterator" "put") "put"
            MNormal Nonpure [TPtr $ TObj cls, valueType] TVoid
     ],
     case mutable of
       Constant -> mempty
       Mutable -> reqInclude $ includeStd "hoppy/iterator.hpp")
  ]

forwardIteratorContents :: IteratorMutability -> Class -> Maybe Type -> ([Ctor], [Method], Reqs)
forwardIteratorContents mutable cls valueTypeMaybe =
  trivialIteratorContents mutable cls valueTypeMaybe `mappend`
  ([],
   [ mkMethod OpIncPre [] $ TRef $ TObj cls
   ],
   mempty)

bidirectionalIteratorContents :: IteratorMutability
                              -> Class
                              -> Maybe Type
                              -> ([Ctor], [Method], Reqs)
bidirectionalIteratorContents mutable cls valueTypeMaybe =
  forwardIteratorContents mutable cls valueTypeMaybe `mappend`
  ([],
   [ mkMethod OpDecPre [] $ TRef $ TObj cls
   ],
   mempty)

randomIteratorContents :: IteratorMutability
                       -> Class
                       -> Maybe Type
                       -> Type
                       -> ([Ctor], [Method], Reqs)
randomIteratorContents mutable cls valueTypeMaybe distanceType =
  bidirectionalIteratorContents mutable cls valueTypeMaybe `mappend`
  ([],
   catMaybes
   [ Just $ mkMethod OpAdd [distanceType] $ TObjToHeap cls
   , Just $ mkMethod OpAddAssign [distanceType] $ TRef $ TObj cls
   , Just $ mkMethod OpSubtract [distanceType] $ TObjToHeap cls
   , Just $ mkMethod' OpSubtract "difference" [TObj cls] distanceType
   , Just $ mkMethod OpSubtractAssign [distanceType] $ TRef $ TObj cls
   , do valueType <- valueTypeMaybe
        Mutable <- Just mutable
        return $ mkMethod' OpArray "at" [distanceType] $ TRef valueType
   , do valueType <- valueTypeMaybe
        return $ mkConstMethod' OpArray "atConst" [distanceType] $ TRef $ TConst valueType
   ],
   mempty)

-- | Adds the contents of a feature to a class.  Does not check for overlap with
-- existing class contents.
classAddFeatures :: [ClassFeature] -> Class -> Class
classAddFeatures features cls =
  foldr (\feature cls' ->
          let (ctors, methods, reqs) = featureContents feature cls'
          in addUseReqs reqs $
             classAddCtors ctors $
             classAddMethods methods cls')
        cls
        features

classAddCtors :: [Ctor] -> Class -> Class
classAddCtors ctors cls =
  if null ctors then cls else cls { classCtors = classCtors cls ++ ctors }

classAddMethods :: [Method] -> Class -> Class
classAddMethods methods cls =
  if null methods then cls else cls { classMethods = classMethods cls ++ methods }
