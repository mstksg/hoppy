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

-- | Support for STL-style iterators.  The functions in this module modify a
-- class to add functionality that is provided by different types of STL
-- iterators.  In the method pseudotypes documented here, the parameter types
-- are 'Type's, and all methods are nonpure.
module Foreign.Hoppy.Generator.Std.Iterator (
  IteratorMutability (..),
  makeTrivialIterator,
  makeForwardIterator,
  makeBidirectionalIterator,
  makeRandomIterator,
  ) where

import Data.Maybe (catMaybes, isJust)
import Foreign.Hoppy.Generator.Spec (
  Class,
  ClassFeature (Assignable, Copyable, Equatable),
  MethodApplicability (MNormal),
  Operator (
    OpAdd,
    OpAddAssign,
    OpArray,
    OpDecPre,
    OpDeref,
    OpIncPre,
    OpSubtract,
    OpSubtractAssign),
  Purity (Nonpure),
  Type,
  addReqIncludes,
  classAddEntities,
  classAddFeatures,
  ident2,
  makeFnMethod,
  mkConstMethod',
  mkCtor,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types
import Foreign.Hoppy.Generator.Std.Internal (includeHelper)

-- | Whether an iterator may be used to modify the underlying collection.
data IteratorMutability = Constant | Mutable
  deriving (Eq, Ord, Show)

-- | @makeTrivialIteartor mutable valueTypeMaybe cls@ turns a class into a
-- trivial iterator, adding:
--
-- * A default constructor named @new@.
--
-- * The class features 'Assignable', 'Copyable', and 'Equatable'.
--
-- * __operator*:__ @getConst :: this -> 'refT' ('constT' valueType)@; if
-- @valueTypeMaybe@ is present.
--
-- * __operator*:__ @get :: this -> 'refT' valueType@; if @valueTypeMaybe@ is
-- present and @mutable@ is 'Mutable'.
--
-- * __*iter = x:__ @put :: this -> valueType -> 'voidT'@; if @valueTypeMaybe@
-- is present and @mutable@ is 'Mutable'.
makeTrivialIterator :: IteratorMutability -> Maybe Type -> Class -> Class
makeTrivialIterator mutable valueTypeMaybe cls =
  (if isJust valueTypeMaybe && mutable == Mutable
   then addReqIncludes [includeHelper "iterator.hpp"]
   else id) $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classAddEntities ents cls
  where ents =
          catMaybes
          [ Just $ mkCtor "new" []
          , do valueType <- valueTypeMaybe
               Mutable <- Just mutable
               return $ mkMethod' OpDeref "get" [] $ refT valueType
          , do valueType <- valueTypeMaybe
               return $ mkConstMethod' OpDeref "getConst" [] $ refT $ constT valueType
          , do valueType <- valueTypeMaybe
               Mutable <- Just mutable
               return $
                 makeFnMethod (ident2 "hoppy" "iterator" "put") "put"
                 MNormal Nonpure [ptrT $ objT cls, valueType] voidT
          ]

-- | Turns a class into a forward iterator, including everything from
-- 'makeTrivialIterator' plus the pre-increment operator:
--
-- * __operator++:__ @next :: this -> 'refT' ('objT' cls)@.
makeForwardIterator :: IteratorMutability -> Maybe Type -> Class -> Class
makeForwardIterator mutable valueTypeMaybe cls =
  classAddEntities ents $
  makeTrivialIterator mutable valueTypeMaybe cls
  where ents =
          [ mkMethod' OpIncPre "next" [] $ refT $ objT cls
          ]

-- | Turns a class into a bidirectional iterator, including everything from
-- 'makeForwardIterator' plus the pre-decrement operator:
--
-- * __operator--:__ @prev :: this -> 'refT' ('objT' cls)@.
makeBidirectionalIterator :: IteratorMutability -> Maybe Type -> Class -> Class
makeBidirectionalIterator mutability valueTypeMaybe cls =
  classAddEntities ents $
  makeForwardIterator mutability valueTypeMaybe cls
  where ents =
          [ mkMethod' OpDecPre "prev" [] $ refT $ objT cls
          ]

-- | @makeRandomIterator mutable valueTypeMaybe distanceType cls@ turns a class
-- into a random iterator, including everything from 'makeBidirectionalIterator'
-- plus some methods:
--
-- * __operator+=:__ @add :: this -> distanceType -> 'refT' ('objT' cls)@.
--
-- * __operator+:__ @plus :: this -> distanceType -> 'toGcT' cls@.
--
-- * __operator-=:__ @subtract :: distanceType -> 'refT' ('objT' cls)@.
--
-- * __operator-:__ @minus :: distanceType -> 'toGcT' cls@.
--
-- * __operator-:__ @difference :: this -> this -> distanceType@.
--
-- * __operator[]:__ @atConst :: distanceType -> 'refT' ('constT' valueType)@;
-- if @valueTypeMaybe@ is present.
--
-- * __operator[]:__ @at :: distanceType -> 'refT' valueType@; if
-- @valueTypeMaybe@ is present and @mutable@ is 'Mutable'.
makeRandomIterator :: IteratorMutability -> Maybe Type -> Type -> Class -> Class
makeRandomIterator mutable valueTypeMaybe distanceType cls =
  classAddEntities ents $
  makeBidirectionalIterator mutable valueTypeMaybe cls
  where ents =
          catMaybes
          [ Just $ mkMethod' OpAdd "plus" [distanceType] $ toGcT $ objT cls
          , Just $ mkMethod' OpAddAssign "add" [distanceType] $ refT $ objT cls
          , Just $ mkMethod' OpSubtract "minus" [distanceType] $ toGcT $ objT cls
          , Just $ mkMethod' OpSubtract "difference" [objT cls] distanceType
          , Just $ mkMethod' OpSubtractAssign "subtract" [distanceType] $ refT $ objT cls
          , do valueType <- valueTypeMaybe
               Mutable <- Just mutable
               return $ mkMethod' OpArray "at" [distanceType] $ refT valueType
          , do valueType <- valueTypeMaybe
               return $ mkConstMethod' OpArray "atConst" [distanceType] $ refT $ constT valueType
          ]
