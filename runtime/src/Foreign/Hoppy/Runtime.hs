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

-- | Runtime support for generated Haskell bindings.
module Foreign.Hoppy.Runtime (
  -- * Primitive types
  CBool (..),
  -- CBool is a newtype for CUChar, so GHC 7.10 (at least) requires reexporting
  -- the CUChar data constructor for CBool to be marshalled in foreign imports.
  CUChar (CUChar),
  coerceIntegral,
  -- * Objects
  CppPtr (..),
  Deletable (..),
  Assignable (..),
  Encodable (..),
  encodeAs,
  Decodable (..),
  decodeAndDelete,
  withCppObj,
  withScopedPtr,
  -- * Containers
  HasContents (..),
  FromContents (..),
  -- * Internal
  CCallback (..),
  freeHaskellFunPtrFunPtr,
  ) where

import Control.Exception (bracket)
import Data.Typeable (Typeable, typeOf)
import Foreign (FunPtr, Ptr, Storable, freeHaskellFunPtr, peek, poke)
import Foreign.C (
  CChar,
  CDouble,
  CFloat,
  CInt,
  CLLong,
  CLong,
  CShort,
  CSize,
  CUChar (CUChar),
  CUInt,
  CULLong,
  CULong,
  CUShort,
  )
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "wrapper" newFreeHaskellFunPtrFunPtr
  :: (FunPtr (IO ()) -> IO ())
  -> IO (FunPtr (FunPtr (IO ()) -> IO ()))

-- | A numeric type representing a C++ boolean.
newtype CBool = CBool CUChar
  deriving (Eq, Integral, Num, Ord, Real, Show, Storable)

instance Bounded CBool where
  minBound = 0
  maxBound = 1

instance Enum CBool where
  fromEnum (CBool n) = fromIntegral n

  toEnum n =
    if n == 0 || n == 1
    then CBool $ fromIntegral n
    else error $ concat ["CBool.toEnum: Invalid value ", show n, "."]

-- | Converts between integral types by going from @a@ to @b@, and also
-- round-tripping the @b@ value back to an @a@ value.  If the two @a@ values
-- don't match, then an error is signalled.
coerceIntegral :: (Integral a, Integral b, Typeable a, Typeable b, Show a) => a -> b
coerceIntegral a =
  let b = fromIntegral a
      a' = fromIntegral b
  in if a' == a
     then b
     else error $ "Conversion from " ++ show (typeOf a) ++ " to " ++
          show (typeOf b) ++ " is not idempotent for value " ++ show a ++ "."

-- | An instance of this class represents a pointer to a C++ object.  All C++
-- classes bound by Hoppy have instances of @CppPtr@.
class CppPtr this where
  -- | Converts to a regular pointer.
  toPtr :: this -> Ptr this

-- | C++ values that can be deleted.  All C++ classes bound by Hoppy have
-- instances of @Deletable@.
class Deletable this where
  -- | Deletes the object with the C++ @delete@ operator.
  delete :: this -> IO ()

-- | A typeclass for references to C++ values that can be assigned to.  This
-- includes raw pointers ('Ptr'), as well as pointers to object types that have
-- an assignment operator (see
-- 'Foreign.Hoppy.Generator.Spec.ClassFeature.Assignable').
class Assignable cppType value where
  -- | @assign x v@ assigns the value @v@ at the location pointed to by @x@.
  assign :: cppType -> value -> IO ()

instance Assignable (Ptr CBool) Bool where
  assign p b = poke p $ if b then 1 else 0

instance Storable a => Assignable (Ptr a) a where
  assign = poke

-- | For a C++ class that also has a native Haskell representation (e.g. value
-- types such as @std::string@), this typeclass allows converting a Haskell
-- value into a C++ object on the heap.  Encoding to both the non-const and
-- const objects is supported.
--
-- Because the functional dependency points in the direction it does, calls of
-- the form @'encode' value@ are ambiguously typed, so 'encodeAs' is provided to
-- resolve the ambiguity.
--
-- Prefer 'withCppObj' over calling 'encode' directly, to manage the lifetime of
-- the object.
--
-- See also 'Decodable'.
class Encodable cppPtrType hsType | cppPtrType -> hsType where
  encode :: hsType -> IO cppPtrType

-- | Takes a dummy argument to help with type resolution of 'encode', a la
-- 'asTypeOf'.  For example, for a C++ pointer type @StdString@ that gets
-- converted to a regular haskell 'String', the expected usage is:
--
-- > str :: String
-- > encodeAs (undefined :: StdString) str
encodeAs :: Encodable cppPtrType hsType => cppPtrType -> hsType -> IO cppPtrType
encodeAs to = fmap (`asTypeOf` to) . encode

-- | A typeclass for converting references to C++ values into Haskell values.
-- What this means depends on the type of C++ value.  Pointers to numeric types
-- and to other pointers (i.e. @'Ptr' ('Ptr' ...)@) are decodable by peeking at
-- the value.
--
-- For a C++ class that also has a native Haskell representation (e.g. value
-- types such as @std::string@), this typeclass allows converting a C++ heap
-- object into a Haskell value based on the defined conversion.  Decoding from
-- both the non-const and const objects is supported.
--
-- See also 'Encodable'.
class Decodable cppPtrType hsType | cppPtrType -> hsType where
  decode :: cppPtrType -> IO hsType

instance Decodable (Ptr CBool) Bool where decode = fmap (/= 0) . peek
instance Decodable (Ptr CChar) CChar where decode = peek
instance Decodable (Ptr CUChar) CUChar where decode = peek
instance Decodable (Ptr CShort) CShort where decode = peek
instance Decodable (Ptr CUShort) CUShort where decode = peek
instance Decodable (Ptr CInt) CInt where decode = peek
instance Decodable (Ptr CUInt) CUInt where decode = peek
instance Decodable (Ptr CLong) CLong where decode = peek
instance Decodable (Ptr CULong) CULong where decode = peek
instance Decodable (Ptr CLLong) CLLong where decode = peek
instance Decodable (Ptr CULLong) CULLong where decode = peek
instance Decodable (Ptr CFloat) CFloat where decode = peek
instance Decodable (Ptr CDouble) CDouble where decode = peek
instance Decodable (Ptr CSize) CSize where decode = peek

instance Decodable (Ptr (Ptr a)) (Ptr a) where decode = peek

-- | Decodes a C++ object to a Haskell value with 'decode', releases the
-- original object with 'delete', then returns the Haskell value.
decodeAndDelete :: (Deletable cppPtrType, Decodable cppPtrType hsType)
                => cppPtrType -> IO hsType
decodeAndDelete ptr = do
  result <- decode ptr
  delete ptr
  return result

-- | Temporarily encodes the Haskell value into a C++ object and passes it to
-- the given function.  When the function finishes, the C++ object is deleted.
withCppObj :: (Deletable cppPtrType, Encodable cppPtrType hsType)
           => hsType -> (cppPtrType -> IO a) -> IO a
withCppObj x = bracket (encode x) delete

-- | @withScopedPtr m f@ runs @m@ to get a pointer, which is given to @f@ to
-- execute.  When @f@ finishes, the pointer is deleted.
withScopedPtr :: Deletable cppPtrType => IO cppPtrType -> (cppPtrType -> IO a) -> IO a
withScopedPtr p = bracket p delete

-- | Containers whose contents can be convered to a list.
--
-- For a container @Cont@ holding values with C-side type @Foo@ and Haskell-side
-- type @Bar@, if the container uses 'Foreign.Hoppy.Generator.Std.ConvertPtr'
-- then the following instances are recommended:
--
-- > instance HasContents ContConst FooConst
-- > instance HasContents Cont Foo
--
-- If the container uses 'Foreign.Hoppy.Generator.Std.ConvertValue' then the
-- following instances are recommended:
--
-- > instance HasContents ContConst Bar
-- > instance HasContents Cont Bar
class HasContents c e | c -> e where
  -- | Extracts the contents of a container, returning the elements in a list.
  toContents :: c -> IO [e]

-- | Containers that can be created from a list.
--
-- For a container @Cont@ holding values with C-side type @Foo@ and Haskell-side
-- type @Bar@, if the container uses 'Foreign.Hoppy.Generator.Std.ConvertPtr'
-- then the following instance is recommended:
--
-- > instance FromContents Cont Foo
--
-- If the container uses 'Foreign.Hoppy.Generator.Std.ConvertValue' then the
-- following instance is recommended:
--
-- > instance HasContents Cont Bar
--
-- No instances for @ContConst@ are needed because it is easy enough to cast the
-- resulting collection to a const pointer.
class FromContents c e | c -> e where
  -- | Creates and returns a new container holding the given elements.
  fromContents :: [e] -> IO c

-- | Internal type that represents a pointer to a C++ callback object (callback
-- impl object, specifically).
newtype CCallback fnHsCType = CCallback (Ptr ())

-- | A global constant function pointer that points to 'freeHaskellFunPtr'.
freeHaskellFunPtrFunPtr :: FunPtr (FunPtr (IO ()) -> IO ())
{-# NOINLINE freeHaskellFunPtrFunPtr #-}
freeHaskellFunPtrFunPtr =
  unsafePerformIO $ newFreeHaskellFunPtrFunPtr freeHaskellFunPtr
