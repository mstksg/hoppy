-- This file is part of Hoppy.
--
-- Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
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

-- | Concrete C++ types.  It is possible to represent invalid C++ types with
-- these functions, but we try to catch these and fail cleanly as much as
-- possible.
module Foreign.Hoppy.Generator.Types (
  -- * Primitive types
  voidT,
  boolT,
  charT,
  ucharT,
  shortT,
  ushortT,
  intT,
  uintT,
  longT,
  ulongT,
  llongT,
  ullongT,
  floatT,
  doubleT,
  int8T,
  int16T,
  int32T,
  int64T,
  word8T,
  word16T,
  word32T,
  word64T,
  ptrdiffT,
  sizeT,
  ssizeT,
  -- * Complex types
  enumT,
  bitspaceT,
  ptrT,
  refT,
  fnT,
  callbackT,
  objT,
  objToHeapT,
  toGcT,
  constT,
  ) where

import Foreign.Hoppy.Generator.Spec.Base

-- | C++ @void@, Haskell @()@.
voidT = Internal_TVoid

-- | C++ @bool@, Haskell 'Bool'.
boolT = Internal_TBool

-- | C++ @char@, Haskell 'Foreign.C.CChar'.
charT = Internal_TChar

-- | C++ @unsigned char@, Haskell 'Foreign.C.CUChar'.
ucharT = Internal_TUChar

-- | C++ @short int@, Haskell 'Foreign.C.CShort'.
shortT = Internal_TShort

-- | C++ @unsigned short int@, Haskell 'Foreign.C.CUShort'.
ushortT = Internal_TUShort

-- | C++ @int@, Haskell 'Foreign.C.CInt'.
intT = Internal_TInt

-- | C++ @unsigned int@, Haskell 'Foreign.C.CUInt'.
uintT = Internal_TUInt

-- | C++ @long int@, Haskell 'Foreign.C.CLong'.
longT = Internal_TLong

-- | C++ @unsigned long int@, Haskell 'Foreign.C.CULong'.
ulongT = Internal_TULong

-- | C++ @long long int@, Haskell 'Foreign.C.CLLong'.
llongT = Internal_TLLong

-- | C++ @unsigned long long int@, Haskell 'Foreign.C.CULLong'.
ullongT = Internal_TULLong

-- | C++ @float@, Haskell 'Foreign.C.CFloat'.
floatT = Internal_TFloat

-- | C++ @double@, Haskell 'Foreign.C.CDouble'.
doubleT = Internal_TDouble

-- | C++ @int8_t@, Haskell 'Data.Int.Int8'.
int8T = Internal_TInt8

-- | C++ @int16_t@, Haskell 'Data.Int.Int16'.
int16T = Internal_TInt16

-- | C++ @int32_t@, Haskell 'Data.Int.Int32'.
int32T = Internal_TInt32

-- | C++ @int64_t@, Haskell 'Data.Int.Int64'.
int64T = Internal_TInt64

-- | C++ @uint8_t@, Haskell 'Data.Word.Word8'.
word8T = Internal_TWord8

-- | C++ @uint16_t@, Haskell 'Data.Word.Word16'.
word16T = Internal_TWord16

-- | C++ @uint32_t@, Haskell 'Data.Word.Word32'.
word32T = Internal_TWord32

-- | C++ @uint64_t@, Haskell 'Data.Word.Word64'.
word64T = Internal_TWord64

-- | C++ @ptrdiff_t@, Haskell 'Foreign.C.CPtrdiff'.
ptrdiffT = Internal_TPtrdiff

-- | C++ @size_t@, Haskell 'Foreign.C.CSize'.
sizeT = Internal_TSize

-- | C++ @ssize_t@, Haskell 'System.Posix.Types.CSsize'.
ssizeT = Internal_TSSize

-- | A C++ @enum@ value.
enumT = Internal_TEnum

-- | A C++ bitspace value.
bitspaceT = Internal_TBitspace

-- | A pointer to another type.
ptrT = Internal_TPtr

-- | A reference to another type.
refT = Internal_TRef

-- | A function taking parameters and returning a value (or 'voidT').  Function
-- pointers must wrap a 'fnT' in a 'ptrT'.
fnT = Internal_TFn

-- | A handle for calling foreign code from C++.
callbackT = Internal_TCallback

-- | An instance of a class.  When used in a parameter or return type and not
-- wrapped in a 'ptrT' or 'refT', this is a by-value object.
objT = Internal_TObj

-- | A special case of 'objT' that is only allowed when passing objects from
-- C++ to a foreign language.  Rather than looking at the object's
-- 'ClassConversion', the object will be copied to the heap, and a pointer to
-- the heap object will be passed.  The object must be copy-constructable.
--
-- __The foreign language owns the pointer, even for callback arguments.__
objToHeapT = Internal_TObjToHeap

-- | This type transfers ownership of the object to the foreign language's
-- garbage collector, and results in a managed pointer in the foreign language.
-- This may only be used in one of the forms below, when passing data from C++
-- to a foreign language (i.e. in a C++ function return type or in a callback
-- argument).  In the first case, the temporary object is copied to the heap,
-- and the result is a managed pointer to the heap object instead of the
-- temporary.
--
-- - @'toGcT' ('objT' cls)@
-- - @'toGcT' ('refT' ('constT' ('objT' cls)))@
-- - @'toGcT' ('refT' ('objT' cls))@
-- - @'toGcT' ('ptrT' ('constT' ('objT' cls)))@
-- - @'toGcT' ('ptrT' ('objT' cls))@
toGcT = Internal_TToGc

-- | A @const@ version of another type.
constT = Internal_TConst
