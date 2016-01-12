-- This file is part of Hoppy.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

{-# LANGUAGE CPP, ScopedTypeVariables #-}

module Main (main) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (when)
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
  )
import Foreign.Hoppy.Runtime (
  CBool (CBool),
  assign,
  decode,
  decodeAndDelete,
  delete,
  encode,
  encodeAs,
  nullptr,
  toPtr,
  withCppObj,
  withScopedPtr,
  )
import Foreign.Hoppy.Test.Basic
import Foreign.Hoppy.Test.Basic.HsBox
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke, sizeOf)
import System.Exit (exitFailure)
import System.Posix.Types (CSsize)
import Test.HUnit (
  Assertion,
  Test (TestList),
  (~:),
  (@?=),
  assert,
  errors,
  failures,
  runTestTT,
  )

main :: IO ()
main = do
  counts <- runTestTT tests
  when (errors counts /= 0 || failures counts /= 0) exitFailure

assertBox :: CInt -> IntBox -> Assertion
assertBox value box = intBox_get box >>= (@?= value)

tests :: Test
tests =
  TestList
  [ functionTests
  , objectTests
  , conversionTests
  , tObjToHeapTests
  , classConversionTests
  , fnMethodTests
  , primitiveTypeSizeTests
  , numericTypePassingTests
  , rawPointerTests
  , inheritanceTests
  ]

functionTests :: Test
functionTests =
  "functions" ~: TestList
  [ "calling a pure function" ~: piapprox @?= 4
  , "calling a non-pure function" ~: piapproxNonpure >>= (@?= 4)
  , "passing an argument" ~: do
    timesTwo 5 @?= 10
    timesTwo (-12) @?= -24
  ]

objectTests :: Test
objectTests =
  "objects" ~: TestList
  [ "creates and deletes an object" ~: do
    box <- intBox_new
    assertBox 0 box
    delete box

  , "calls an overloaded constructor" ~: do
    box <- intBox_newWithValue (-1)
    assertBox (-1) box
    delete box

  , "null pointers are in fact null" ~: do
    toPtr (nullptr :: IntBox) @?= nullPtr
    toPtr (nullptr :: IntBoxConst) @?= nullPtr

  , "non-null pointers in fact aren't null" ~:
    withScopedPtr intBox_new $ assert . (nullPtr /=) . toPtr
  ]


conversionTests :: Test
conversionTests =
  "object conversion" ~: TestList
  [ "encode" ~: do
    box <- encode $ HsBox 3 :: IO IntBox
    assertBox 3 box
    delete box

  , "encodeAs" ~: do
    box <- encodeAs (undefined :: IntBox) $ HsBox 4
    assertBox 4 box
    delete box

  , "decode" ~: do
    box <- intBox_newWithValue 5
    hsBox <- decode box
    delete box
    hsBox @?= HsBox 5

  , "withCppObj" ~: withCppObj (HsBox 6) $ assertBox 6
  ]

tObjToHeapTests :: Test
tObjToHeapTests =
  "TObjToHeap" ~: TestList
  [ "PtrCtr functions properly" ~: do
    ptrCtr_resetCounters
    getCounts >>= (@?= (0, 0))
    p <- ptrCtr_new
    getCounts >>= (@?= (1, 0))
    p2 <- ptrCtr_new
    getCounts >>= (@?= (2, 0))
    delete p
    getCounts >>= (@?= (2, 1))
    delete p2
    getCounts >>= (@?= (2, 2))

  , "returning from C++ by value" ~: do
    ptrCtr_resetCounters
    withScopedPtr givePtrCtrByValue $ \_ -> do
      (ctors, dtors) <- getCounts
      dtors @?= ctors - 1
    (ctors, dtors) <- getCounts
    dtors @?= ctors

  , "passing to Haskell callbacks by value" ~: do
    ptrCtr_resetCounters
    givePtrCtrByValueToCallback $ \p -> do
      getCounts >>= (@?= 2) . fst
      delete p
    getCounts >>= (@?= (2, 2))

  -- We don't repeat the below tests passing by reference and pointer here,
  -- because the semantics are exactly the same.
  ]
  where getCounts = (,) <$> ptrCtr_getConstructionCount <*> ptrCtr_getDestructionCount

classConversionTests :: Test
classConversionTests =
  "class conversion" ~: TestList
  [ "passing to C++" ~: TestList
    [ "by value" ~: do
      withCppObj (HsBox 1) $ \(box :: IntBox) -> getBoxValueByValue box >>= (@?= 1)
      withCppObj (HsBox 2) $ \(box :: IntBoxConst) -> getBoxValueByValue box >>= (@?= 2)
    , "by reference" ~:
      withCppObj (HsBox 3) $ \(box :: IntBox) -> getBoxValueByRef box >>= (@?= 3)
      -- Passing a const pointer to a non-const reference is disallowed.
    , "by constant reference" ~: do
      withCppObj (HsBox 5) $ \(box :: IntBox) -> getBoxValueByRefConst box >>= (@?= 5)
      withCppObj (HsBox 6) $ \(box :: IntBoxConst) -> getBoxValueByRefConst box >>= (@?= 6)
    , "by pointer" ~:
      withCppObj (HsBox 7) $ \(box :: IntBox) -> getBoxValueByPtr box >>= (@?= 7)
      -- Passing a const pointer to a non-const pointer is disallowed.
    , "by constant pointer" ~: do
      withCppObj (HsBox 9) $ \(box :: IntBox) -> getBoxValueByPtrConst box >>= (@?= 9)
      withCppObj (HsBox 10) $ \(box :: IntBoxConst) -> getBoxValueByPtrConst box >>= (@?= 10)
    ]

  , "returning from C++" ~: TestList
    [ "by value" ~:
      (makeBoxByValue 1 :: IO HsBox) >>= (@?= HsBox 1)
    , "by reference" ~:
      (makeBoxByRef 2 :: IO IntBox) >>= decodeAndDelete >>= (@?= HsBox 2)
    , "by constant reference" ~:
      (makeBoxByRefConst 3 :: IO IntBoxConst) >>= decodeAndDelete >>= (@?= HsBox 3)
    , "by pointer" ~:
      (makeBoxByPtr 4 :: IO IntBox) >>= decodeAndDelete >>= (@?= HsBox 4)
    , "by constant pointer" ~:
      (makeBoxByPtrConst 5 :: IO IntBoxConst) >>= decodeAndDelete >>= (@?= HsBox 5)
    ]

  , "passing to Haskell callbacks" ~: TestList
    [ "by value" ~:
      getBoxValueByValueCallbackDriver (\(hsBox :: HsBox) -> return $ getHsBox hsBox) 1 >>= (@?= 1)
    , "by reference" ~:
      getBoxValueByRefCallbackDriver (\(box :: IntBox) -> intBox_get box) 2 >>= (@?= 2)
    , "by constant reference" ~:
      getBoxValueByRefConstCallbackDriver (\(box :: IntBoxConst) -> intBox_get box) 3 >>= (@?= 3)
    , "by pointer" ~:
      getBoxValueByPtrCallbackDriver (\(box :: IntBox) -> intBox_get box) 4 >>= (@?= 4)
    , "by constant pointer" ~:
      getBoxValueByPtrConstCallbackDriver (\(box :: IntBoxConst) -> intBox_get box) 5 >>= (@?= 5)
    ]

  , "returning from Haskell callbacks" ~: TestList
    [ "by value" ~:
      makeBoxByValueCallbackDriver (return . HsBox) 1 >>= (@?= 1)
    , "by reference" ~:
      makeBoxByRefCallbackDriver intBox_newWithValue 2 >>= (@?= 2)  -- We must go deeper.
    , "by constant reference" ~:
      makeBoxByRefConstCallbackDriver (fmap toIntBoxConst . intBox_newWithValue) 3 >>= (@?= 3)
    , "by pointer" ~:
      makeBoxByPtrCallbackDriver intBox_newWithValue 4 >>= (@?= 4)
    , "by constant pointer" ~:
      makeBoxByPtrConstCallbackDriver (fmap toIntBoxConst . intBox_newWithValue) 5 >>= (@?= 5)
    ]
  ]

fnMethodTests :: Test
fnMethodTests =
  "function methods" ~: TestList
  [ "MNormal" ~: withScopedPtr (intBoxWithFnMethods_new 5) $ \box ->
    intBoxWithFnMethods_getValue box >>= (@?= 5)

  , "MConst" ~: withScopedPtr (intBoxWithFnMethods_new 5) $ \box -> do
    intBoxWithFnMethods_getValueConst box >>= (@?= 5)
    intBoxWithFnMethods_getValueConst (toIntBoxWithFnMethodsConst box) >>= (@?= 5)

  , "MStatic" ~: intBoxWithFnMethods_double 7 @?= 14
  ]

primitiveTypeSizeTests :: Test
primitiveTypeSizeTests =
  "primitive type sizes" ~: TestList
  [ -- TBool has special conversions, since the size of Haskell's Bool can be
    -- more than one byte.  We also test the special conversion logic here.
    "TBool" ~: fromIntegral sizeOfBool @?= 1
  , "TBool true->true conversion" ~: isTrue True >>= (@?= True)
  , "TBool false->false conversion" ~: isTrue False >>= (@?= False)
  , "TBool true->false conversion" ~: isFalse True >>= (@?= False)
  , "TBool false->true conversion" ~: isFalse False >>= (@?= True)
  , "TChar" ~: fromIntegral sizeOfChar @?= sizeOf (undefined :: CChar)
  , "TShort" ~: fromIntegral sizeOfShort @?= sizeOf (undefined :: CShort)
  , "TInt" ~: fromIntegral sizeOfInt @?= sizeOf (undefined :: CInt)
  , "TLong" ~: fromIntegral sizeOfLong @?= sizeOf (undefined :: CLong)
  , "TLLong" ~: fromIntegral sizeOfLLong @?= sizeOf (undefined :: CLLong)
  , "TFloat" ~: fromIntegral sizeOfFloat @?= sizeOf (undefined :: CFloat)
  , "TDouble" ~: fromIntegral sizeOfDouble @?= sizeOf (undefined :: CDouble)
  , "TPtrdiff" ~: fromIntegral sizeOfPtrdiff @?= sizeOf (undefined :: CPtrdiff)
  , "TSize" ~: fromIntegral sizeOfSize @?= sizeOf (undefined :: CSize)
  , "TSSize" ~: fromIntegral sizeOfSSize @?= sizeOf (undefined :: CSsize)
  ]

numericTypePassingTests :: Test
numericTypePassingTests =
  "numeric type passing" ~: TestList
  [ "TInt" ~: doubleInt 5 @?= 10
  , "TLong" ~: doubleLong (-5) @?= -10
  , "TFloat" ~: doubleFloat 0.5 @?= 1
  , "TDouble" ~: doubleDouble 1867 @?= 3734
  , "TInt8" ~: doubleInt8 63 @?= 126
  , "TInt32" ~: doubleInt32 (-1073741824) @?= minBound -- -2147483648
  , "TWord16" ~: doubleUInt16 32767 @?= 65534
  , "TWord64" ~: doubleUInt64 9223372036854775807 @?= 18446744073709551614
  ]

rawPointerTests :: Test
rawPointerTests =
  "raw pointers" ~: TestList
  [ "can read and write a bool*" ~: do
    p <- getBoolPtr
    peek p >>= (@?= CBool 0)
    decode p >>= (@?= False)
    assign p True
    peek p >>= (@?= CBool 1)
    decode p >>= (@?= True)

  , "can read and write an int*" ~: do
    p <- getIntPtr
    peek p >>= (@?= 23)
    decode p >>= (@?= 23)
    assign p (34 :: CInt)
    peek p >>= (@?= 34)
    decode p >>= (@?= 34)

  , "can read an int**" ~: do
    pp <- getIntPtrPtr
    p <- decode pp
    decode p >>= (@?= 1234)

  , "can read a IntBox**" ~: do
    pp <- getIntBoxPtrPtr
    p <- decode pp
    intBox_get p >>= (@?= 1010)

  , "can pass an int*" ~:
    alloca $ \p -> do
      poke p (32 :: CInt)
      doubleIntPtr p
      peek p >>= (@?= 64)

  , "can pass an int**" ~:
    alloca $ \p ->
    alloca $ \pp -> do
      poke p (32 :: CInt)
      poke pp p
      doubleIntPtrPtr pp
      peek p >>= (@?= 64)

  , "can pass an int&" ~:
    alloca $ \p -> do
      poke p (32 :: CInt)
      doubleIntRef p
      peek p >>= (@?= 64)
  ]

inheritanceTests :: Test
inheritanceTests =
  "inheritance tests" ~: TestList
  [ "multiple inheritance" ~: do
    c <- inheritanceC_new
    inheritanceA_aFoo c >>= (@?= 1)  -- From the first superclass.
    inheritanceA_aBar c >>= (@?= 20)  -- From the first superclass, overridden.
    inheritanceB_bFoo c >>= (@?= 30)  -- From the second superclass, overridden.
  ]
