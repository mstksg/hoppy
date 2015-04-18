module Foreign.Cppop.Runtime.Support (
  CppPtr (..),
  CCallback (..),
  decodeAndFreeCString,
  coerceIntegral,
  ) where

import Data.Typeable (Typeable, typeOf)
import Foreign (Ptr, free)
import Foreign.C (CString, peekCString)

class CppPtr this where
  toPtr :: this -> Ptr this

  -- | Deletes an object with the C++ @delete@ operator.
  delete :: this -> IO ()

-- | Internal type that represents a pointer to a C++ callback object (callback
-- impl object, specifically).
newtype CCallback fnHsCType = CCallback (Ptr ())

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
