module Foreign.Cppop.Runtime.Support (
  CppPtr (..),
  CCallback (..),
  decodeAndFreeCString,
  ) where

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
