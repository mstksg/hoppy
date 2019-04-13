-- This file is part of Hoppy.
--
-- Copyright 2015-2019 Bryan Gardiner <bog@khumba.net>
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
  -- * Qualifiers
  constT,
  -- * Primtive types
  voidT,
  ptrT,
  refT,
  fnT,
  fnT',
  -- * Numeric types
  boolT,
  boolT',
  charT,
  ucharT,
  shortT,
  ushortT,
  intT,
  intT',
  uintT,
  longT,
  ulongT,
  llongT,
  ullongT,
  floatT,
  floatT',
  doubleT,
  doubleT',
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
  -- ** Custom numeric types
  makeNumericType,
  convertByCoercingIntegral,
  convertByCoercingFloating,
  -- * Complex types
  manualT,
  callbackT,
  enumT,
  objT,
  objToHeapT,
  toGcT,
  ) where

import {-# SOURCE #-} qualified Foreign.Hoppy.Generator.Language.Haskell as LH
import {-# SOURCE #-} Foreign.Hoppy.Generator.Spec.Callback (callbackT)
import {-# SOURCE #-} Foreign.Hoppy.Generator.Spec.Enum (enumT)
import {-# SOURCE #-} Foreign.Hoppy.Generator.Spec.Function (fnT, fnT')
import Foreign.Hoppy.Generator.Spec.Base
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

-- | C++ @void@, Haskell @()@.
voidT = Internal_TVoid

-- | C++ @bool@, Haskell 'Bool'.
--
-- C++ has sizeof(bool) == 1, whereas Haskell can > 1, so we have to convert.
boolT =
  makeNumericType "bool" mempty
  (do LH.addImports hsImportForPrelude
      return $ HsTyCon $ UnQual $ HsIdent "HoppyP.Bool")
  (Just $ do LH.addImports hsImportForForeignC
             return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CBool")
  (CustomConversion $ do
     LH.addImports $ mconcat [hsImport1 "Prelude" "(.)",
                              hsImportForPrelude]
     LH.sayLn "\\x -> HoppyP.return $ if x then 1 else 0")
  (CustomConversion $ do
     LH.addImports $ mconcat [hsImports "Prelude" ["(.)", "(/=)"],
                              hsImportForPrelude]
     LH.sayLn "(HoppyP.return . (/= 0))")

-- | C++ @bool@, Haskell 'Foreign.C.CBool'.
boolT' =
  makeNumericType "bool" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CBool")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @char@, Haskell 'Foreign.C.CChar'.
charT =
  makeNumericType "char" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CChar")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @unsigned char@, Haskell 'Foreign.C.CUChar'.
ucharT =
  makeNumericType "unsigned char" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CUChar")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @short int@, Haskell 'Foreign.C.CShort'.
shortT =
  makeNumericType "short" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CShort")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @unsigned short int@, Haskell 'Foreign.C.CUShort'.
ushortT =
  makeNumericType "unsigned short" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CUShort")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @int@, Haskell 'Int'.  See also 'intT''.
intT =
  makeNumericType "int" mempty
  (do LH.addImports hsImportForPrelude
      return $ HsTyCon $ UnQual $ HsIdent "HoppyP.Int")
  (Just $ do LH.addImports hsImportForForeignC
             return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CInt")
  convertByCoercingIntegral convertByCoercingIntegral

-- | C++ @int@, Haskell 'Foreign.C.CInt'.  See also 'intT'.
intT' =
  makeNumericType "int" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CInt")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @unsigned int@, Haskell 'Foreign.C.CUInt'.
uintT =
  makeNumericType "unsigned int" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CUInt")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @long int@, Haskell 'Foreign.C.CLong'.
longT =
  makeNumericType "long" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CLong")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @unsigned long int@, Haskell 'Foreign.C.CULong'.
ulongT =
  makeNumericType "unsigned long" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CULong")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @long long int@, Haskell 'Foreign.C.CLLong'.
llongT =
  makeNumericType "long long" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CLLong")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @unsigned long long int@, Haskell 'Foreign.C.CULLong'.
ullongT =
  makeNumericType "unsigned long long" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CULLong")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @float@, Haskell 'Prelude.Float'.  See also 'floatT''.
floatT =
  makeNumericType "float" mempty
  (do LH.addImports hsImportForPrelude
      return $ HsTyCon $ UnQual $ HsIdent "HoppyP.Float")
  (Just $ do LH.addImports hsImportForForeignC
             return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CFloat")
  convertByCoercingFloating convertByCoercingFloating

-- | C++ @float@, Haskell 'Foreign.C.CFloat'.  See also 'floatT'.
floatT' =
  makeNumericType "float" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "Foreign.C.CFloat")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @double@, Haskell 'Prelude.Double'.  See also 'doubleT''.
doubleT =
  makeNumericType "double" mempty
  (do LH.addImports hsImportForPrelude
      return $ HsTyCon $ UnQual $ HsIdent "HoppyP.Double")
  (Just $ do LH.addImports hsImportForForeignC
             return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CDouble")
  convertByCoercingFloating convertByCoercingFloating

-- | C++ @double@, Haskell 'Foreign.C.CDouble'.  See also 'doubleT'.
doubleT' =
  makeNumericType "double" mempty
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "Foreign.C.CDouble")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @int8_t@, Haskell 'Data.Int.Int8'.
int8T =
  makeNumericType "int8_t" (reqInclude $ includeStd "cstdint")
  (do LH.addImports hsImportForInt
      return $ HsTyCon $ UnQual $ HsIdent "HoppyDI.Int8")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @int16_t@, Haskell 'Data.Int.Int16'.
int16T =
  makeNumericType "int16_t" (reqInclude $ includeStd "cstdint")
  (do LH.addImports hsImportForInt
      return $ HsTyCon $ UnQual $ HsIdent "HoppyDI.Int16")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @int32_t@, Haskell 'Data.Int.Int32'.
int32T =
  makeNumericType "int32_t" (reqInclude $ includeStd "cstdint")
  (do LH.addImports hsImportForInt
      return $ HsTyCon $ UnQual $ HsIdent "HoppyDI.Int32")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @int64_t@, Haskell 'Data.Int.Int64'.
int64T =
  makeNumericType "int64_t" (reqInclude $ includeStd "cstdint")
  (do LH.addImports hsImportForInt
      return $ HsTyCon $ UnQual $ HsIdent "HoppyDI.Int64")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @uint8_t@, Haskell 'Data.Word.Word8'.
word8T =
  makeNumericType "uint8_t" (reqInclude $ includeStd "cstdint")
  (do LH.addImports hsImportForWord
      return $ HsTyCon $ UnQual $ HsIdent "HoppyDW.Word8")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @uint16_t@, Haskell 'Data.Word.Word16'.
word16T =
  makeNumericType "uint16_t" (reqInclude $ includeStd "cstdint")
  (do LH.addImports hsImportForWord
      return $ HsTyCon $ UnQual $ HsIdent "HoppyDW.Word16")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @uint32_t@, Haskell 'Data.Word.Word32'.
word32T =
  makeNumericType "uint32_t" (reqInclude $ includeStd "cstdint")
  (do LH.addImports hsImportForWord
      return $ HsTyCon $ UnQual $ HsIdent "HoppyDW.Word32")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @uint64_t@, Haskell 'Data.Word.Word64'.
word64T =
  makeNumericType "uint64_t" (reqInclude $ includeStd "cstdint")
  (do LH.addImports hsImportForWord
      return $ HsTyCon $ UnQual $ HsIdent "HoppyDW.Word64")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @ptrdiff_t@, Haskell 'Foreign.C.CPtrdiff'.
ptrdiffT =
  makeNumericType "ptrdiff_t" (reqInclude $ includeStd "cstddef")
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CPtrdiff")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @size_t@, Haskell 'Foreign.C.CSize'.
sizeT =
  makeNumericType "size_t" (reqInclude $ includeStd "cstddef")
  (do LH.addImports hsImportForForeignC
      return $ HsTyCon $ UnQual $ HsIdent "HoppyFC.CSize")
  Nothing BinaryCompatible BinaryCompatible

-- | C++ @ssize_t@, Haskell 'System.Posix.Types.CSsize'.
ssizeT =
  makeNumericType "ssize_t" (reqInclude $ includeStd "cstddef")
  (do LH.addImports hsImportForSystemPosixTypes
      return $ HsTyCon $ UnQual $ HsIdent "HoppySPT.CSsize")
  Nothing BinaryCompatible BinaryCompatible

-- | Builds a new numeric type definition.
--
-- For convenience, 'convertByCoercingIntegral' and 'convertByCoercingFloating'
-- may be used as conversion methods, for both 'ConversionMethod' arguments this
-- function takes.
makeNumericType ::
     String
     -- ^ The name of the C++ type.
  -> Reqs
     -- ^ Includes necessary to use the C++ type.
  -> LH.Generator HsType
     -- ^ Generator for rendering the Haskell type to be used, along with any
     -- required imports.  See 'conversionSpecHaskellHsType'.
  -> Maybe (LH.Generator HsType)
     -- ^ If there is a Haskell type distinct from the previous argument to be
     -- used for passing over the FFI boundary, then provide it here.  See
     -- 'conversionSpecHaskellCType'.
  -> ConversionMethod (LH.Generator ())
     -- ^ Method to use to convert a Haskell value to a value to be passed over
     -- the FFI.  See 'conversionSpecHaskellToCppFn'.
  -> ConversionMethod (LH.Generator ())
     -- ^ Method to use to convert a value received over the FFI into a Haskell
     -- value.  See 'conversionSpecHaskellFromCppFn'.
  -> Type
makeNumericType cppName cppReqs hsTypeGen hsCTypeGen convertToCpp convertFromCpp =
  Internal_TManual spec
  where spec =
          (makeConversionSpec cppName $ makeConversionSpecCpp cppName $ return cppReqs)
          { conversionSpecHaskell =
              Just $ makeConversionSpecHaskell
                hsTypeGen
                hsCTypeGen
                convertToCpp
                convertFromCpp
          }

-- | Conversion method for passing a numeric values to and from Haskell by using
-- @Foreign.Hoppy.Runtime.coerceIntegral@.
convertByCoercingIntegral :: ConversionMethod (LH.Generator ())
convertByCoercingIntegral = CustomConversion $ do
  LH.addImports $ mconcat [hsImport1 "Prelude" "(.)",
                           hsImportForPrelude,
                           hsImportForRuntime]
  LH.sayLn "HoppyP.return . HoppyFHR.coerceIntegral"

-- | Conversion method for passing a numeric values to and from Haskell by using
-- 'realToFrac'.
convertByCoercingFloating :: ConversionMethod (LH.Generator ())
convertByCoercingFloating = CustomConversion $ do
  LH.addImports $ mconcat [hsImport1 "Prelude" "(.)",
                           hsImportForPrelude]
  LH.sayLn "HoppyP.return . HoppyP.realToFrac"

-- | A pointer to another type.
ptrT = Internal_TPtr

-- | A reference to another type.
refT = Internal_TRef

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

-- | Constructs a type from a specification of how to convert values.
manualT :: ConversionSpec -> Type
manualT = Internal_TManual

-- | A @const@ version of another type.
constT = Internal_TConst
