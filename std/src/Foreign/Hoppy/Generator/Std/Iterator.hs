-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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
  Type (TConst, TObj, TObjToHeap, TPtr, TRef, TVoid),
  addReqIncludes,
  classAddCtors,
  classAddMethods,
  ident2,
  makeFnMethod,
  mkConstMethod',
  mkCtor,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
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
-- * __operator*:__ @getConst :: this -> 'TRef' ('TConst' valueType)@; if
-- @valueTypeMaybe@ is present.
--
-- * __operator*:__ @get :: this -> 'TRef' valueType@; if @valueTypeMaybe@ is
-- present and @mutable@ is 'Mutable'.
--
-- * __*iter = x:__ @put :: this -> valueType -> 'TVoid'@; if @valueTypeMaybe@
-- is present and @mutable@ is 'Mutable'.
makeTrivialIterator :: IteratorMutability -> Maybe Type -> Class -> Class
makeTrivialIterator mutable valueTypeMaybe cls =
  (if isJust valueTypeMaybe && mutable == Mutable
   then addReqIncludes [includeHelper "iterator.hpp"]
   else id) $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classAddCtors ctors $
  classAddMethods methods cls
  where ctors =
          [ mkCtor "new" []
          ]
        methods =
          catMaybes
          [ do valueType <- valueTypeMaybe
               Mutable <- Just mutable
               return $ mkMethod' OpDeref "get" [] $ TRef valueType
          , do valueType <- valueTypeMaybe
               return $ mkConstMethod' OpDeref "getConst" [] $ TRef $ TConst valueType
          , do valueType <- valueTypeMaybe
               Mutable <- Just mutable
               return $
                 makeFnMethod (ident2 "hoppy" "iterator" "put") "put"
                 MNormal Nonpure [TPtr $ TObj cls, valueType] TVoid
          ]

-- | Turns a class into a forward iterator, including everything from
-- 'makeTrivialIterator' plus the pre-increment operator:
--
-- * __operator++:__ @next :: this -> 'TRef' ('TObj' cls)@.
makeForwardIterator :: IteratorMutability -> Maybe Type -> Class -> Class
makeForwardIterator mutable valueTypeMaybe cls =
  classAddMethods methods $
  makeTrivialIterator mutable valueTypeMaybe cls
  where methods =
          [ mkMethod' OpIncPre "next" [] $ TRef $ TObj cls
          ]

-- | Turns a class into a bidirectional iterator, including everything from
-- 'makeForwardIterator' plus the pre-decrement operator:
--
-- * __operator--:__ @prev :: this -> 'TRef' ('TObj' cls)@.
makeBidirectionalIterator :: IteratorMutability -> Maybe Type -> Class -> Class
makeBidirectionalIterator mutability valueTypeMaybe cls =
  classAddMethods methods $
  makeForwardIterator mutability valueTypeMaybe cls
  where methods =
          [ mkMethod' OpDecPre "prev" [] $ TRef $ TObj cls
          ]

-- | @makeRandomIterator mutable valueTypeMaybe distanceType cls@ turns a class
-- into a random iterator, including everything from 'makeBidirectionalIterator'
-- plus some methods:
--
-- * __operator+=:__ @add :: this -> distanceType -> 'TRef' ('TObj' cls)@.
--
-- * __operator+:__ @plusNew :: this -> distanceType -> 'TObjToHeap' cls@.
--
-- * __operator-=:__ @subtract :: distanceType -> 'TRef' ('TObj' cls)@.
--
-- * __operator-:__ @minusNew :: distanceType -> 'TObjToHeap' cls@.
--
-- * __operator-:__ @difference :: this -> this -> distanceType@.
--
-- * __operator[]:__ @atConst :: distanceType -> 'TRef' ('TConst' valueType)@;
-- if @valueTypeMaybe@ is present.
--
-- * __operator[]:__ @at :: distanceType -> 'TRef' valueType@; if
-- @valueTypeMaybe@ is present and @mutable@ is 'Mutable'.
makeRandomIterator :: IteratorMutability -> Maybe Type -> Type -> Class -> Class
makeRandomIterator mutable valueTypeMaybe distanceType cls =
  classAddMethods methods $
  makeBidirectionalIterator mutable valueTypeMaybe cls
  where methods =
          catMaybes
          [ Just $ mkMethod' OpAdd "plusNew" [distanceType] $ TObjToHeap cls
          , Just $ mkMethod' OpAddAssign "add" [distanceType] $ TRef $ TObj cls
          , Just $ mkMethod' OpSubtract "minusNew" [distanceType] $ TObjToHeap cls
          , Just $ mkMethod' OpSubtract "difference" [TObj cls] distanceType
          , Just $ mkMethod' OpSubtractAssign "subtract" [distanceType] $ TRef $ TObj cls
          , do valueType <- valueTypeMaybe
               Mutable <- Just mutable
               return $ mkMethod' OpArray "at" [distanceType] $ TRef valueType
          , do valueType <- valueTypeMaybe
               return $ mkConstMethod' OpArray "atConst" [distanceType] $ TRef $ TConst valueType
          ]
