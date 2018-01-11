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

{-# LANGUAGE CPP, ScopedTypeVariables, ViewPatterns #-}

module Main (main) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception (evaluate)
import Control.Monad ((<=<), forM_, unless, when)
import Data.Bits ((.&.), (.|.), xor)
import Data.IORef (newIORef, readIORef, writeIORef)
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
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, withForeignPtr)
import Foreign.Hoppy.Runtime (
  CBool,
  CppException,
  CppThrowable,
  UnknownCppException,
  assign,
  catchCpp,
  decode,
  decodeAndDelete,
  delete,
  encode,
  encodeAs,
  nullptr,
  throwCpp,
  toGc,
  toPtr,
  touchCppPtr,
  withCppObj,
  withScopedFunPtr,
  withScopedPtr,
  )
import Foreign.Hoppy.Test.Basic
import Foreign.Hoppy.Test.Basic.HsBox
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke, sizeOf)
import System.Exit (exitFailure)
import System.Mem (performGC)
import System.Posix.Types (CSsize)
import Test.HUnit (
  Assertion,
  Test (TestList),
  (~:),
  (@?=),
  assert,
  assertBool,
  assertFailure,
  errors,
  failures,
  runTestTT,
  )

main :: IO ()
main = do
  counts <- runTestTT tests
  when (errors counts /= 0 || failures counts /= 0) exitFailure

assertBox :: Int -> IntBox -> Assertion
assertBox value box = intBox_get box >>= (@?= value)

tests :: Test
tests =
  TestList
  [ functionTests
  , objectTests
  , objectGcTests
  , conversionTests
  , tObjToHeapTests
  , classConversionTests
  , fnMethodTests
  , primitiveTypeSizeTests
  , numericTypePassingTests
  , rawPointerTests
  , inheritanceTests
  , enumTests
  , bitspaceTests
  , exceptionTests
  ]

functionTests :: Test
functionTests =
  "functions" ~: TestList
  [ "calling a pure function" ~: piapprox @?= 4

  , "calling a non-pure function" ~: piapproxNonpure >>= (@?= 4)

  , "passing an argument" ~: do
    timesTwo 5 @?= 10
    timesTwo (-12) @?= -24

  , "passing function pointers" ~: TestList
    [ "with primitive types" ~:
      withScopedFunPtr (longCallback_newFunPtr $ return . timesTwo) $ \timesTwoPtr ->
      takesLongFn timesTwoPtr 3 @?= -6

    , "uses C types" ~:
      withScopedFunPtr (intBoxCallback_newFunPtr $
                        intBox_newWithValue . (* 3) <=< intBox_get) $ \funPtr ->
      takesIntBoxFn funPtr 2 >>= (@?= 30)
    ]
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

  , "undeletable instances compile" ~: do
    x <- undeletable_getInstance
    assert $ x /= nullptr

  , "classes with alternate entity prefixes" ~: do
    altPrefixClass_foo >>= (@?= 3)
    withScopedPtr ctorWithNoPrefix methodWithNoPrefix >>= (@?= 4)
  ]

objectGcTests :: Test
objectGcTests =
  "object garbage collection" ~: TestList
  [ "objects are not collected by default" ~: do
    ptrCtr_resetCounters
    withScopedPtr ptrCtr_new $ \p -> do
      ptrCtr_redButton p
      performGC
      readCounts >>= (@?= (1, 0))
    readCounts >>= (@?= (1, 1))

  , "toGc creates collectable pointers" ~: do
    ptrCtr_resetCounters
    p <- toGc =<< ptrCtr_new
    ptrCtr_redButton p
    performGC
    readCounts >>= (@?= (1, 1))

  , "(toGcT . objT) creates collectable pointers" ~: do
    ptrCtr_resetCounters
    p <- ptrCtr_newGcedObj
    ptrCtr_redButton p
    performGC
    (news, dels) <- readCounts
    dels @?= news

  , "(toGcT . refT . constT . objT) creates collectable pointers" ~: do
    ptrCtr_resetCounters
    p <- ptrCtr_newGcedRefConst
    ptrCtr_redButton p
    performGC
    readCounts >>= (@?= (1, 1))

  , "(toGcT . refT . objT) creates collectable pointers" ~: do
    ptrCtr_resetCounters
    p <- ptrCtr_newGcedRef
    ptrCtr_redButton p
    performGC
    readCounts >>= (@?= (1, 1))

  , "(toGcT . ptrT . constT . objT) creates collectable pointers" ~: do
    ptrCtr_resetCounters
    p <- ptrCtr_newGcedPtrConst
    ptrCtr_redButton p
    performGC
    readCounts >>= (@?= (1, 1))

  , "(toGcT . ptrT . objT) creates collectable pointers" ~: do
    ptrCtr_resetCounters
    p <- ptrCtr_newGcedPtr
    ptrCtr_redButton p
    performGC
    readCounts >>= (@?= (1, 1))

  , "casted ForeignPtrs contribute to an object's lifetime" ~: do
    ptrCtr_resetCounters
    p <- ptrCtr_newGcedPtr
    case p of
      PtrCtrGc (castForeignPtr -> fptr :: ForeignPtr ()) _ -> do
        withForeignPtr fptr $ \_ -> do
          performGC
          -- p is collectable now, but fptr should be keeping the counter alive.
          -- If you replace "withForeignPtr" with "flip ($)", this read returns
          -- (1, 1) instead of (1, 0).
          readCounts >>= (@?= (1, 0))
        performGC
        readCounts >>= (@?= (1, 1))
      _ -> assertFailure $
           concat ["Expected a managed pointer, not ", show p, "."]

  , "managed and unmanaged pointers are equatable" ~: do
    unmanaged <- ptrCtr_new
    managed <- toGc unmanaged
    managed @?= unmanaged

  , "managed and unmanaged pointers aren't ordered by state" ~: do
    unmanaged0 <- ptrCtr_new
    managed0 <- toGc unmanaged0
    unmanaged1 <- ptrCtr_new
    managed1 <- toGc unmanaged1
    compare unmanaged0 managed0 @?= EQ
    compare unmanaged1 managed1 @?= EQ
    compare unmanaged0 managed1 @?= compare managed0 unmanaged1
    compare unmanaged1 managed0 @?= compare managed1 unmanaged0
  ]
  where readCounts = (,) <$> ptrCtr_getConstructionCount <*> ptrCtr_getDestructionCount

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

  , "ClassConversionToHeap" ~: do
    let readCounts = (,) <$>
                     ptrCtrWithToHeapConversion_getConstructionCount <*>
                     ptrCtrWithToHeapConversion_getDestructionCount
    ptrCtrWithToHeapConversion_resetCounters
    p <- ptrCtrWithToHeapConversion_newHeapObj
    readCounts >>= assertNAlive 1
    delete p
    readCounts >>= assertNAlive 0

  , "ClassConversionToGc" ~: do
    ptrCtrWithToGcConversion_resetCounters
    p <- ptrCtrWithToGcConversion_newGcedObj
    readCounts >>= assertNAlive 1
    performGC
    touchCppPtr p
    performGC
    readCounts >>= assertNAlive 0
  ]
  where readCounts = (,) <$>
                     ptrCtrWithToGcConversion_getConstructionCount <*>
                     ptrCtrWithToGcConversion_getDestructionCount

        assertNAlive n calls@(ctorCalls, _) =
          calls @?= (ctorCalls, ctorCalls - n)

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
  [ -- boolT has special conversions, since the size of Haskell's Bool can be
    -- more than one byte.  We also test the special conversion logic here.
    "boolT" ~: fromIntegral sizeOfBool @?= 1
  , "boolT true->true conversion" ~: isTrue True >>= (@?= True)
  , "boolT false->false conversion" ~: isTrue False >>= (@?= False)
  , "boolT true->false conversion" ~: isFalse True >>= (@?= False)
  , "boolT false->true conversion" ~: isFalse False >>= (@?= True)
  , "charT" ~: fromIntegral sizeOfChar @?= sizeOf (undefined :: CChar)
  , "shortT" ~: fromIntegral sizeOfShort @?= sizeOf (undefined :: CShort)
  , "intT" ~: fromIntegral sizeOfInt @?= sizeOf (undefined :: CInt)
  , "longT" ~: fromIntegral sizeOfLong @?= sizeOf (undefined :: CLong)
  , "llongT" ~: fromIntegral sizeOfLLong @?= sizeOf (undefined :: CLLong)
  , "floatT" ~: fromIntegral sizeOfFloat @?= sizeOf (undefined :: CFloat)
  , "doubleT" ~: fromIntegral sizeOfDouble @?= sizeOf (undefined :: CDouble)
  , "ptrdiffT" ~: fromIntegral sizeOfPtrdiff @?= sizeOf (undefined :: CPtrdiff)
  , "sizeT" ~: fromIntegral sizeOfSize @?= sizeOf (undefined :: CSize)
  , "ssizeT" ~: fromIntegral sizeOfSSize @?= sizeOf (undefined :: CSsize)
  ]

numericTypePassingTests :: Test
numericTypePassingTests =
  "numeric type passing" ~: TestList
  [ "intT" ~: doubleInt 5 @?= 10
  , "longT" ~: doubleLong (-5) @?= -10
  , "floatT" ~: doubleFloat 0.5 @?= 1
  , "doubleT" ~: doubleDouble 1867 @?= 3734
  , "int8T" ~: doubleInt8 63 @?= 126
  , "int32T" ~: doubleInt32 (-1073741824) @?= minBound -- -2147483648
  , "word16T" ~: doubleUInt16 32767 @?= 65534
  , "word64T" ~: doubleUInt64 9223372036854775807 @?= 18446744073709551614
  ]

rawPointerTests :: Test
rawPointerTests =
  "raw pointers" ~: TestList
  [ "can read and write a bool*" ~: do
    p <- getBoolPtr :: IO (Ptr CBool)
    peek p >>= (@?= 0)
    decode p >>= (@?= False)
    assign p True
    peek p >>= (@?= 1)
    decode p >>= (@?= True)

  , "can read and write an int*" ~: do
    p <- getIntPtr :: IO (Ptr CInt)
    peek p >>= (@?= 23)
    decode p >>= (@?= 23)
    assign p (34 :: CInt)
    peek p >>= (@?= 34)
    decode p >>= (@?= 34)

  , "can read an int**" ~: do
    pp <- getIntPtrPtr :: IO (Ptr (Ptr CInt))
    p <- decode pp
    decode p >>= (@?= 1234)

  , "can read a IntBox**" ~: do
    pp <- getIntBoxPtrPtr :: IO (Ptr (Ptr IntBox))
    p <- decode =<< decode pp
    intBox_get p >>= (@?= 1010)

  , "can pass an IntBox**" ~:
    withScopedPtr (intBox_newWithValue 3) $ \box ->
    alloca $ \(p :: Ptr (Ptr IntBox)) -> do
      poke p $ toPtr box
      doubleIntBoxPtrPtr p
      intBox_get box >>= (@?= 6)

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

  , "upcasting" ~: withScopedPtr inheritanceC_new $ \c -> do
    let ca = toInheritanceA c
        cb = toInheritanceBConst c
    inheritanceA_aFoo ca >>= (@?= 1)
    inheritanceA_aBar ca >>= (@?= 20)
    inheritanceB_bFoo cb >>= (@?= 30)
    let cac = downToInheritanceC ca
        cbc = downToInheritanceCConst cb
    cac @?= c
    cbc @?= castInheritanceCToConst c
  ]

enumTests :: Test
enumTests =
  "enums" ~: TestList
  [ "fromEnum" ~: do
    fromEnum BetterBool_True @?= 0
    fromEnum BetterBool_False @?= 1
    fromEnum BetterBool_FileNotFound @?= 4

  , "toEnum" ~: do
    toEnum 0 @?= BetterBool_True
    toEnum 1 @?= BetterBool_False
    toEnum 4 @?= BetterBool_FileNotFound

  , "calling a C++ function" ~: do
    betterBoolId BetterBool_True @?= BetterBool_True
    betterBoolId BetterBool_False @?= BetterBool_False
    betterBoolId BetterBool_FileNotFound @?= BetterBool_FileNotFound

  , "calling a callback" ~: do
    takesBetterBoolCallback rot BetterBool_True @?= BetterBool_False
    takesBetterBoolCallback rot BetterBool_False @?= BetterBool_FileNotFound
    takesBetterBoolCallback rot BetterBool_FileNotFound @?= BetterBool_True
  ]
  where rot BetterBool_True = return BetterBool_False
        rot BetterBool_False = return BetterBool_FileNotFound
        rot BetterBool_FileNotFound = return BetterBool_True

bitspaceTests :: Test
bitspaceTests =
  "bitspaces" ~: TestList
  [ "to CInt" ~: do
    fromBetterBools betterBools_True @?= 0
    fromBetterBools betterBools_False @?= 1
    fromBetterBools betterBools_FileNotFound @?= 4

  , "from Int" ~: do
    toBetterBools (0 :: Int) @?= betterBools_True
    toBetterBools (1 :: Int) @?= betterBools_False
    toBetterBools (4 :: Int) @?= betterBools_FileNotFound

  , "from CInt" ~: do
    toBetterBools (0 :: CInt) @?= betterBools_True
    toBetterBools (1 :: CInt) @?= betterBools_False
    toBetterBools (4 :: CInt) @?= betterBools_FileNotFound

  , "from enum" ~: do
    toBetterBools BetterBool_True @?= betterBools_True
    toBetterBools BetterBool_False @?= betterBools_False
    toBetterBools BetterBool_FileNotFound @?= betterBools_FileNotFound

  , "from bitspace" ~: do
    toBetterBools betterBools_True @?= betterBools_True
    toBetterBools betterBools_False @?= betterBools_False
    toBetterBools betterBools_FileNotFound @?= betterBools_FileNotFound

  , "Bits instance" ~: do
    betterBools_True .|. betterBools_False @?= betterBools_False
    betterBools_True .&. betterBools_FileNotFound @?= betterBools_True

  , "calling a C++ function" ~: do
    betterBoolsId betterBools_True @?= betterBools_True
    betterBoolsId betterBools_False @?= betterBools_False
    betterBoolsId betterBools_FileNotFound @?= betterBools_FileNotFound

  , "calling a callback" ~: do
    takesBetterBoolsCallback op betterBools_True @?= betterBools_False .|. betterBools_FileNotFound
    takesBetterBoolsCallback op betterBools_False @?= betterBools_FileNotFound
    takesBetterBoolsCallback op betterBools_FileNotFound @?= betterBools_False
  ]
  where op x = return $ x `xor` toBetterBools (5 :: Int)

exceptionTests :: Test
exceptionTests =
  "exceptions" ~: TestList
  [ "catching the thrown type" ~: TestList
    [ "throwing BaseException, catching BaseException" ~:
      expectException throwsBaseException (undefined :: BaseException)
    , "throwing ReadException, catching ReadException" ~:
      expectException throwsReadException (undefined :: ReadException)
    , "throwing BaseException, should not catch ReadException" ~:
      expectExceptionButNot throwsBaseException
      (undefined :: BaseException) (undefined :: ReadException)
    ]

  , "catching supertypes of thrown types" ~: TestList
    [ "throwing FileException, catching BaseException" ~:
      expectException throwsFileException (undefined :: FileException)
    , "throwing ReadException, catching BaseException" ~:
      expectException throwsReadException (undefined :: ReadException)
    , "throwing WriteException, catching BaseException" ~:
      expectException throwsWriteException (undefined :: WriteException)
    ]

  , "catching (...)" ~: TestList
    [ "throwing BaseException, catching (...)" ~:
      expectException throwsBaseException (undefined :: UnknownCppException)
    , "throwing ReadException, catching (...)" ~:
      expectException throwsReadException (undefined :: UnknownCppException)
    , "throwing types Hoppy doesn't catch" ~:
      forM_ [0..3] $ \i -> do
        threw <- newIORef False
        catchCpp (throwsAny i) $ \(_ :: UnknownCppException) ->
          writeIORef threw True
        readIORef threw >>= assertBool "Expected an exception to be thrown."
    ]

  , let testThrowFromHaskellToCpp :: CppThrowable e => IO e -> Int -> IO ()
        testThrowFromHaskellToCpp ctor expectedResult =
          invokeThrowingCallback (ctor >>= throwCpp) >>= (@?= expectedResult)
    in "throwing exceptions" ~: TestList
       [ "throwing BaseException to C++" ~: testThrowFromHaskellToCpp baseException_new 1
       , "throwing FileException to C++" ~: testThrowFromHaskellToCpp fileException_new 2
       , "throwing ReadException to C++" ~: testThrowFromHaskellToCpp readException_new 3
       , "throwing WriteException to C++" ~: testThrowFromHaskellToCpp writeException_new 4

       , "throwing a GCed object" ~: do
         testThrowFromHaskellToCpp (toGc =<< baseException_new) 1
         performGC  -- This should not crash.

       , "throwing and catching all in Haskell" ~: do
         okVar <- newIORef False
         e@(ReadException p) <- readException_new
         catchCpp (throwCpp e) $ \e' -> case e' of
           -- The exception should have been assigned to the garbage collector,
           -- and the underlying object should be the same.
           ReadExceptionGc _ p' -> do
             p' @?= p
             writeIORef okVar True
           _ -> return ()
         readIORef okVar >>= assertBool "Didn't catch exception as expected."
       ]

  , "exception lifecycle" ~: TestList
    [ "a caught exception object is GCable" ~: do
      ptrCtr_resetCounters
      threw <- newIORef False
      catchCpp throwsPtrCtr $ \(e :: PtrCtr) -> do
        writeIORef threw True
        performGC
        readCounts >>= assertNAlive 1
        touchCppPtr e
        performGC
        readCounts >>= assertNAlive 0
      readIORef threw >>= assertBool "Expected an exception to be thrown."

    , "catching a SomeCppException as (...) deletes the object" ~: do
      ptrCtr_resetCounters
      threw <- newIORef False
      catchCpp throwsPtrCtr $ \(e :: UnknownCppException) -> do
        writeIORef threw True
        performGC
        readCounts >>= assertNAlive 0
        _ <- evaluate e
        return ()
      readIORef threw >>= assertBool "Expected an exception to be thrown."
    ]

  , "catching exceptions from value-returning functions" ~: TestList
    [ "returning a bool" ~:
      expectException throwingReturnBool (undefined :: BaseException)

    , "returning an int" ~:
      expectException throwingReturnInt (undefined :: BaseException)

    , "returning an IntBox by value" ~:
      expectException throwingReturnIntBox (undefined :: BaseException)

    , -- This test is pretty cool, we call a C++ function which calls a Haskell
      -- callback which throws, and the exception is propagated back out to the
      -- original Haskell caller.
      "returning an IntBox from a callback" ~:
      expectException
        (throwingMakeBoxByValueCallbackDriver (\_ -> baseException_new >>= throwCpp) 0)
        (undefined :: BaseException)
    ]
  ]
  where expectException :: CppException e => IO a -> e -> IO ()
        expectException action excType = do
          threwRef <- newIORef False
          catchCpp (action >> return ()) $ \e ->
            asTypeOf e excType `seq` writeIORef threwRef True
          threw <- readIORef threwRef
          unless threw $ assertFailure "Expected to catch an exception."

        expectExceptionButNot :: (CppException e1, CppException e2) => IO a -> e1 -> e2 -> IO ()
        expectExceptionButNot action expectedExcType unexpectedExcType = do
          caughtExpectedRef <- newIORef False
          caughtUnexpectedRef <- newIORef False
          catchCpp (catchCpp (action >> return ()) $
                    \e -> asTypeOf e unexpectedExcType `seq` writeIORef caughtUnexpectedRef True) $
            \e -> asTypeOf e expectedExcType `seq` writeIORef caughtExpectedRef True

          caughtExpected <- readIORef caughtExpectedRef
          caughtUnexpected <- readIORef caughtUnexpectedRef
          case (caughtExpected, caughtUnexpected) of
            (True, False) -> return ()
            (False, True) -> assertFailure "Caught an exception of unexpected type."
            (False, False) -> assertFailure "Expected to catch an exception."
            (True, True) -> assertFailure "Through magic, we caught two exceptions."

        readCounts = (,) <$>
                     ptrCtrWithToGcConversion_getConstructionCount <*>
                     ptrCtrWithToGcConversion_getDestructionCount

        assertNAlive n calls@(ctorCalls, _) =
          calls @?= (ctorCalls, ctorCalls - n)
