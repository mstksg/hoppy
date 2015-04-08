module Foreign.Cppop.Runtime.Support (
  CppPtr (..),
  decodeAndFreeCString,
  ) where

import Foreign (Ptr, free)
import Foreign.C (CString, peekCString)

class CppPtr this where
  toPtr :: this -> Ptr this

decodeAndFreeCString :: CString -> IO String
decodeAndFreeCString cstr = do
  str <- peekCString cstr
  free cstr
  return str
