module Foreign.Cppop.Runtime.Support (
  CppPtr (..),
  Encodable (..),
  encodeAs,
  Decodable (..),
  CCallback (..),
  freeHaskellFunPtrFunPtr,
  decodeAndFreeCString,
  coerceIntegral,
  ) where

import Data.Typeable (Typeable, typeOf)
import Foreign (FunPtr, Ptr, free, freeHaskellFunPtr)
import Foreign.C (CString, peekCString)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "wrapper" newFreeHaskellFunPtrFunPtr
  :: (FunPtr (IO ()) -> IO ())
  -> IO (FunPtr (FunPtr (IO ()) -> IO ()))

-- | An instance of this class represents a pointer to a C++ object.
class CppPtr this where
  -- | Deletes the object with the C++ @delete@ operator.
  delete :: this -> IO ()

  -- | Internal function to convert to a pointer.  Used in generated bindings;
  -- do not use otherwise.
  toPtr :: this -> Ptr this

-- | For a C++ class that also has a native Haskell representation (e.g. value
-- types such as @std::string@), this typeclass allows converting a Haskell
-- value into a C++ object on the heap.  Encoding to both the non-const and
-- const objects is supported.
--
-- Because the functional dependency points in the direction it does, calls of
-- the form @'encode' value@ are ambiguously typed, so 'encodeAs' is provided to
-- resolve the ambiguity.
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

-- | For a C++ class that also has a native Haskell representation (e.g. value
-- types such as @std::string@), this typeclass allows converting a C++ heap
-- object into a Haskell value.  Decoding from both the non-const and const
-- objects is supported.
--
-- See also 'Encodable'.
class Decodable cppPtrType hsType | cppPtrType -> hsType where
  decode :: cppPtrType -> IO hsType

-- | Internal type that represents a pointer to a C++ callback object (callback
-- impl object, specifically).
newtype CCallback fnHsCType = CCallback (Ptr ())

freeHaskellFunPtrFunPtr :: FunPtr (FunPtr (IO ()) -> IO ())
{-# NOINLINE freeHaskellFunPtrFunPtr #-}
freeHaskellFunPtrFunPtr =
  unsafePerformIO $ newFreeHaskellFunPtrFunPtr freeHaskellFunPtr

decodeAndFreeCString :: CString -> IO String
decodeAndFreeCString cstr = do
  str <- peekCString cstr
  free cstr
  return str

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
