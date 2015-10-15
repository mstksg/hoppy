module Main where

import Control.Monad (when)
import Data.Bits ((.|.))
import Foreign.C (castCCharToChar, castCharToCChar)
import Foreign.Hoppy.Runtime.Support (toPtr)
import Foreign.Hoppy.Test.Flob
import Foreign.Hoppy.Test.Flub
import System.Exit (exitFailure)
import Test.HUnit (
  Test (TestList),
  (~:),
  (@?=),
  errors,
  failures,
  runTestTT,
  )

main :: IO ()
main = do
  counts <- runTestTT tests
  when (errors counts /= 0 || failures counts /= 0) exitFailure

-- The main circularity test is that everything builds, but we poke at the
-- generated code too to make sure it runs.

tests :: Test
tests =
  TestList
  [ "circular modules execute" ~: do
    flubVar_set $ castCharToCChar 'A'
    fmap castCCharToChar flubVar_get >>= (@?= 'A')
    fmap castCCharToChar flubVarConst_get >>= (@?= 'Z')
    flobObj <- flobClass_new
    flubObj <- flubClass_new
    takesFlobValues flobObj
    takesFlubValues flubObj FlubEnum_OptionA flubBitspace_OptionB
    fmap toPtr returnsFlubClass >>= (@?= toPtr flubClass_null)
    returnsFlubEnum >>= (@?= FlubEnum_OptionB)
    returnsFlubBitspace >>= (@?= flubBitspace_OptionA .|. flubBitspace_OptionC)
  ]
