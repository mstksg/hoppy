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
  Copyable (..),
  Encodable (..),
  encodeAs,
  Decodable (..),
  decodeAndDelete,
  withCppObj,
  withScopedPtr,
  withScopedFunPtr,
  -- * Exceptions
  CppException (..),
  CppThrowable (..),
  catchCpp,
  throwCpp,
  UnknownCppException,
  -- * Containers
  HasContents (..),
  FromContents (..),
  -- * Internal
  CCallback (..),
  freeHaskellFunPtrFunPtr,
  ExceptionId (..),
  SomeCppException (..),
  internalHandleExceptions,
  internalHandleCallbackExceptions,
  ExceptionDb (..),
  ExceptionClassInfo (..),
  ) where

import Control.Exception (Exception, bracket, catch, throwIO)
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Typeable (Typeable, typeOf)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign (
  ForeignPtr,
  FunPtr,
  Ptr,
  Storable,
  alloca,
  freeHaskellFunPtr,
  nullPtr,
  peek,
  poke,
  touchForeignPtr,
  )
import Foreign.C (
  CChar,
  CDouble,
  CFloat,
  CInt,
  CLLong,
  CLong,
  CPtrdiff,
  CShort,
  CSize,
  CUChar (CUChar),
  CUInt,
  CULLong,
  CULong,
  CUShort,
  )
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (CSsize)
import Unsafe.Coerce (unsafeCoerce)

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
          show (typeOf b) ++ " does not preserve the value " ++ show a ++ "."

-- | An instance of this class represents a handle (a pointer) to a C++ object.
-- All C++ classes bound by Hoppy have instances of @CppPtr@.  The lifetime of
-- such an object can optionally be managed by the Haskell garbage collector.
-- Handles returned from constructors are unmanaged, and 'toGc' converts an
-- unmanaged handle to a managed one.  'delete' must not be called on managed
-- handles.
class CppPtr this where
  -- | Polymorphic null pointer handle.
  nullptr :: this

  -- | Runs an IO action on the 'Ptr' underlying this handle.  Equivalent to
  -- 'ForeignPtr.withForeignPtr' for managed handles: the 'Ptr' is only
  -- guaranteed to be valid until the action returns.  There is no such
  -- restriction for unmanaged handles, but of course the object must still be
  -- alive to be used.
  withCppPtr :: this -> (Ptr this -> IO a) -> IO a

  -- | Converts to a regular pointer.  For objects managed by the garbage
  -- collector, this comes with the warnings associated with
  -- 'ForeignPtr.Unsafe.unsafeForeignPtrToPtr', namely that the object may be
  -- collected immediately after this function returns unless there is a
  -- 'touchCppPtr' call later on.
  toPtr :: this -> Ptr this

  -- | Equivalent to 'ForeignPtr.touchForeignPtr' for managed handles.  Has no
  -- effect on unmanaged handles.
  touchCppPtr :: this -> IO ()

-- | C++ values that can be deleted.  By default, C++ classes bound by Hoppy are
-- assumed to be deletable, so they get instances of @Deletable@.
class Deletable this where
  -- | Deletes the object with the C++ @delete@ operator.
  delete :: this -> IO ()

  -- | Converts a handle to one managed by the garbage collector.  A __new__
  -- managed handle is returned, and existing handles __including__ the
  -- argument remain unmanaged, becoming invalid once all managed handles are
  -- unreachable.  Calling this on an already managed handle has no effect and
  -- the argument is simply returned.  It is no longer safe to call 'delete' on
  -- the given object after calling this function.  It is also not safe to call
  -- this function on unmanaged handles for a single object multiple times: the
  -- object will get deleted more than once.
  --
  -- Up- and downcasting managed handles keeps the object alive correctly.
  toGc :: this -> IO this

-- | A typeclass for references to C++ values that can be assigned to.  This
-- includes raw pointers ('Ptr'), as well as handles for object types that have
-- an assignment operator (see
-- 'Foreign.Hoppy.Generator.Spec.ClassFeature.Assignable').
class Assignable cppType value where
  -- | @assign x v@ assigns the value @v@ at the location pointed to by @x@.
  assign :: cppType -> value -> IO ()

instance Assignable (Ptr CBool) Bool where
  assign p b = poke p $ if b then 1 else 0

instance Assignable (Ptr CInt) Int where
  assign p i = poke p $ coerceIntegral i

instance Assignable (Ptr CFloat) Float where
  assign p x = poke p $ realToFrac x

instance Assignable (Ptr CDouble) Double where
  assign p x = poke p $ realToFrac x

instance Storable a => Assignable (Ptr a) a where
  assign = poke

-- | A typeclass for creating copies of C++ objects.  Every C++ class with a
-- copy constructor will have two instances:
--
-- > instance Copyable Foo Foo
-- > instance Copyable FooConst Foo
class Copyable from to | from -> to where
  copy :: from -> IO to

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
-- 'asTypeOf'.  For example, for a handle type @StdString@ that gets converted
-- to a regular haskell 'String', the expected usage is:
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
instance Decodable (Ptr CInt) Int where decode = fmap coerceIntegral . peek
instance Decodable (Ptr CUInt) CUInt where decode = peek
instance Decodable (Ptr CLong) CLong where decode = peek
instance Decodable (Ptr CULong) CULong where decode = peek
instance Decodable (Ptr CLLong) CLLong where decode = peek
instance Decodable (Ptr CULLong) CULLong where decode = peek
instance Decodable (Ptr CFloat) Float where decode = fmap realToFrac . peek
instance Decodable (Ptr CDouble) Double where decode = fmap realToFrac . peek
instance Decodable (Ptr Int8) Int8 where decode = peek
instance Decodable (Ptr Int16) Int16 where decode = peek
instance Decodable (Ptr Int32) Int32 where decode = peek
instance Decodable (Ptr Int64) Int64 where decode = peek
instance Decodable (Ptr Word8) Word8 where decode = peek
instance Decodable (Ptr Word16) Word16 where decode = peek
instance Decodable (Ptr Word32) Word32 where decode = peek
instance Decodable (Ptr Word64) Word64 where decode = peek
instance Decodable (Ptr CPtrdiff) CPtrdiff where decode = peek
instance Decodable (Ptr CSize) CSize where decode = peek
instance Decodable (Ptr CSsize) CSsize where decode = peek

instance Decodable (Ptr (Ptr CBool)) (Ptr CBool) where decode = peek
instance Decodable (Ptr (Ptr CChar)) (Ptr CChar) where decode = peek
instance Decodable (Ptr (Ptr CUChar)) (Ptr CUChar) where decode = peek
instance Decodable (Ptr (Ptr CShort)) (Ptr CShort) where decode = peek
instance Decodable (Ptr (Ptr CUShort)) (Ptr CUShort) where decode = peek
instance Decodable (Ptr (Ptr CInt)) (Ptr CInt) where decode = peek
instance Decodable (Ptr (Ptr CUInt)) (Ptr CUInt) where decode = peek
instance Decodable (Ptr (Ptr CLong)) (Ptr CLong) where decode = peek
instance Decodable (Ptr (Ptr CULong)) (Ptr CULong) where decode = peek
instance Decodable (Ptr (Ptr CLLong)) (Ptr CLLong) where decode = peek
instance Decodable (Ptr (Ptr CULLong)) (Ptr CULLong) where decode = peek
instance Decodable (Ptr (Ptr CFloat)) (Ptr CFloat) where decode = peek
instance Decodable (Ptr (Ptr CDouble)) (Ptr CDouble) where decode = peek
instance Decodable (Ptr (Ptr Int8)) (Ptr Int8) where decode = peek
instance Decodable (Ptr (Ptr Int16)) (Ptr Int16) where decode = peek
instance Decodable (Ptr (Ptr Int32)) (Ptr Int32) where decode = peek
instance Decodable (Ptr (Ptr Int64)) (Ptr Int64) where decode = peek
instance Decodable (Ptr (Ptr Word8)) (Ptr Word8) where decode = peek
instance Decodable (Ptr (Ptr Word16)) (Ptr Word16) where decode = peek
instance Decodable (Ptr (Ptr Word32)) (Ptr Word32) where decode = peek
instance Decodable (Ptr (Ptr Word64)) (Ptr Word64) where decode = peek
instance Decodable (Ptr (Ptr CPtrdiff)) (Ptr CPtrdiff) where decode = peek
instance Decodable (Ptr (Ptr CSize)) (Ptr CSize) where decode = peek
instance Decodable (Ptr (Ptr CSsize)) (Ptr CSsize) where decode = peek

instance Decodable (Ptr (Ptr (Ptr a))) (Ptr (Ptr a)) where decode = peek

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

-- | @withScopedPtr m f@ runs @m@ to get a handle, which is given to @f@ to
-- execute.  When @f@ finishes, the handle is deleted (via 'bracket' and
-- 'delete').
withScopedPtr :: Deletable cppPtrType => IO cppPtrType -> (cppPtrType -> IO a) -> IO a
withScopedPtr p = bracket p delete

-- | @withScopedFunPtr m f@ runs @m@ to get a 'FunPtr', which is given to @f@ to
-- execute.  When @f@ finishes, the 'FunPtr' is deleted (via 'bracket' and
-- 'freeHaskellFunPtr').  This is useful in conjunction with function pointers
-- created via generated callback functions.
withScopedFunPtr :: IO (FunPtr a) -> (FunPtr a -> IO b) -> IO b
withScopedFunPtr p = bracket p freeHaskellFunPtr

-- | A unique identifier for a C++ class.  The representation is internal to
-- Hoppy.
newtype ExceptionId = ExceptionId CInt
                    deriving (Eq, Ord, Show)

-- | A typeclass for C++ values that are catchable as exceptions.  C++ classes that
-- have been declared to be used as exceptions have instances of this class.
-- Unlike 'CppThrowable', 'UnknownCppException' is also an instance of this
-- typeclass.
class CppException e where
  -- | Internal.  Returns metadata about the exception.
  cppExceptionInfo :: e -> ExceptionClassInfo

  -- | Internal.  Constructs a handle from a GC-managed object's raw pointers.
  cppExceptionBuild :: ForeignPtr () -> Ptr () -> e

  -- | Internal.  Constructs a GC-managed handle from an unmanaged raw pointer.
  cppExceptionBuildToGc :: Ptr () -> IO e

-- | A typeclass for C++ values that are throwable as exceptions.  C++ classes that
-- have been declared to be used as exceptions have instances of this class.
class CppException e => CppThrowable e where
  -- | Internal.  Creates a 'throw'able exception from a C++ handle.
  toSomeCppException :: e -> SomeCppException

-- | Catches a C++ exception, similar to 'catch'.  Catching an exception class
-- will also catch subtypes of the class, per normal C++ exception semantics.
-- Catching 'UnknownCppException' will catch all C++ exceptions, but will
-- provide no information about the caught exception.  Exceptions caught with
-- this function are GC-managed heap objects; you do not need to manually delete
-- them.
catchCpp :: forall a e. CppException e => IO a -> (e -> IO a) -> IO a
catchCpp action handler = do
  let expectedId = exceptionClassId $ cppExceptionInfo (undefined :: e)

  catch action $ \caughtEx -> case caughtEx of
    SomeCppException classInfo caughtFPtr caughtPtr ->
      if expectedId == exceptionClassId (cppExceptionInfo UnknownCppException)
      then do
        -- We're catching the top-level exception type, so we're done with the
        -- actual exception object.  If it's not garbage collected, delete it.
        case caughtFPtr of
          Nothing -> exceptionClassDelete classInfo caughtPtr
          Just _ -> return ()

        -- UnknownCppException is the only type with ID 1, so e ~ UnknownCppException.
        handler $ unsafeCoerce UnknownCppException

      else do
        -- Attempt to get a pointer for the type we're hoping to catch.
        let maybeUpcastedPtr :: Maybe (Ptr ())
            maybeUpcastedPtr =
              if expectedId == exceptionClassId classInfo
              then Just caughtPtr
              else case M.lookup expectedId $ exceptionClassUpcasts classInfo of
                Just upcast -> Just $ upcast caughtPtr
                Nothing -> Nothing

        -- Call the handler, ensuring that the handle we pass is GCed.
        case maybeUpcastedPtr of
          Just upcastedPtr -> handler =<< case caughtFPtr of
            Just fptr -> return $ cppExceptionBuild fptr upcastedPtr
            Nothing -> cppExceptionBuildToGc upcastedPtr
          Nothing -> throwIO caughtEx

    SomeUnknownCppException ->
      if expectedId == exceptionClassId (cppExceptionInfo UnknownCppException)
      then handler $ unsafeCoerce UnknownCppException  -- Same as above, this is safe.
      else throwIO caughtEx

-- | Takes ownership of a C++ object, and throws it as a Haskell exception.
-- This can be caught in Haskell with 'catchCpp', or propagated to C++ when
-- within a callback that is marked as handling exceptions.
throwCpp :: CppThrowable e => e -> IO a
throwCpp = throwIO . toSomeCppException

-- | A top type for C++ exceptions.  Catching this type with 'catchCpp' will
-- catch all C++ exceptions.  (You still have to declare what exceptions can be
-- thrown from each function, to make exceptions pass through the gateway
-- properly.)
data UnknownCppException = UnknownCppException

instance CppException UnknownCppException where
  cppExceptionInfo _ = ExceptionClassInfo
    { exceptionClassId = ExceptionId 1
    , exceptionClassName = "<Unknown C++ exception>"
    , exceptionClassUpcasts = M.empty
    , exceptionClassDelete = error "UnknownCppException.exceptionClassDelete: Should not get here."
    , exceptionClassCopy = error "UnknownCppException.exceptionClassCopy: Should not get here."
    , exceptionClassToGc = error "UnknownCppException.exceptionClassToGc: Should not get here."
    }

  cppExceptionBuild _ _ =
    error "Internal error: cppExceptionBuild called for UnknownCppException"

  cppExceptionBuildToGc _ =
    error "Internal error: cppExceptionBuildToGc called for UnknownCppException"

-- | Internal.  Holds an arbitrary 'CppException'.
--
-- Do not catch this with 'catch'; this can leak exception objects.  Always use
-- 'catchCpp' to catch C++ exceptions.
data SomeCppException =
    SomeCppException ExceptionClassInfo (Maybe (ForeignPtr ())) (Ptr ())
  | SomeUnknownCppException
  deriving (Typeable)

instance Exception SomeCppException

instance Show SomeCppException where
  show (SomeCppException info _ _) =
    "<SomeCppException " ++ exceptionClassName info ++ ">"
  show SomeUnknownCppException =
    exceptionClassName $ cppExceptionInfo (undefined :: UnknownCppException)

-- | Internal.  Wraps a call to a C++ gateway function, and provides propagation
-- of C++ exceptions to Haskell.
internalHandleExceptions :: ExceptionDb -> (Ptr CInt -> Ptr (Ptr ()) -> IO a) -> IO a
internalHandleExceptions (ExceptionDb db) f =
  alloca $ \excIdPtr ->
  alloca $ \excPtrPtr -> do
  result <- f excIdPtr excPtrPtr
  excId <- peek excIdPtr
  case excId of
    0 -> return result
    1 -> throwIO SomeUnknownCppException
    _ -> do excPtr <- peek excPtrPtr
            case M.lookup (ExceptionId excId) db of
              Just info -> do
                fptr <- exceptionClassToGc info excPtr
                throwIO $ SomeCppException info (Just fptr) excPtr
              Nothing ->
                fail $
                "internalHandleExceptions: Received C++ exception with unknown exception ID " ++
                show excId ++ "."

-- | Internal.  Wraps a call to a Haskell function while invoking a callback,
-- and provides propagation of C++ exceptions back into C++.
internalHandleCallbackExceptions :: CppDefault a => Ptr CInt -> Ptr (Ptr ()) -> IO a -> IO a
internalHandleCallbackExceptions excIdPtr excPtrPtr doCall = do
  -- Indicate no exception unless we catch something.
  poke excIdPtr 0

  catch doCall $ \caughtEx -> case caughtEx of
    SomeCppException classInfo caughtFPtr caughtPtr -> do
      let ExceptionId excId = exceptionClassId classInfo
      poke excIdPtr excId
      poke excPtrPtr =<< case caughtFPtr of
        Just fptr -> do
          copiedPtr <- exceptionClassCopy classInfo caughtPtr
          touchForeignPtr fptr
          return copiedPtr
        Nothing -> return caughtPtr
      return cppDefault

    SomeUnknownCppException ->
      fail "Can't propagate unknown C++ exception from Haskell to C++."

-- | Internal.  A database of information about exceptions an interface uses.
newtype ExceptionDb = ExceptionDb (Map ExceptionId ExceptionClassInfo)

-- | Internal.  Information about a C++ exception class.
data ExceptionClassInfo = ExceptionClassInfo
  { exceptionClassId :: ExceptionId
  , exceptionClassName :: String
  , exceptionClassUpcasts :: Map ExceptionId (Ptr () -> Ptr ())
    -- ^ This maps ancestor classes' exception IDs to functions that cast
    -- pointers from the current type to the ancestor type.
  , exceptionClassDelete :: Ptr () -> IO ()
    -- ^ Deletes the object.
  , exceptionClassCopy :: Ptr () -> IO (Ptr ())
    -- ^ Invokes the object's copy constructor.
  , exceptionClassToGc :: Ptr () -> IO (ForeignPtr ())
    -- ^ Assigns the object to the Haskell garbage collector, a la 'toGc'.
  }

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

-- | Internal.  Provides default values.
class CppDefault a where
  cppDefault :: a

instance CppDefault () where cppDefault = ()
instance CppDefault CBool where cppDefault = 0
instance CppDefault CChar where cppDefault = 0
instance CppDefault CUChar where cppDefault = 0
instance CppDefault CShort where cppDefault = 0
instance CppDefault CUShort where cppDefault = 0
instance CppDefault CInt where cppDefault = 0
instance CppDefault CUInt where cppDefault = 0
instance CppDefault CLong where cppDefault = 0
instance CppDefault CULong where cppDefault = 0
instance CppDefault CLLong where cppDefault = 0
instance CppDefault CULLong where cppDefault = 0
instance CppDefault CFloat where cppDefault = 0
instance CppDefault CDouble where cppDefault = 0
instance CppDefault Int8 where cppDefault = 0
instance CppDefault Int16 where cppDefault = 0
instance CppDefault Int32 where cppDefault = 0
instance CppDefault Int64 where cppDefault = 0
instance CppDefault Word8 where cppDefault = 0
instance CppDefault Word16 where cppDefault = 0
instance CppDefault Word32 where cppDefault = 0
instance CppDefault Word64 where cppDefault = 0
instance CppDefault CPtrdiff where cppDefault = 0
instance CppDefault CSize where cppDefault = 0
instance CppDefault CSsize where cppDefault = 0

instance CppDefault (Ptr a) where cppDefault = nullPtr
