module Main where

import Control.Monad (forM_, when)
import Foreign.Cppop.Test.Basic
import Test.HUnit (
  Test (TestCase, TestList),
  (~:),
  (@?=),
  errors,
  failures,
  runTestTT,
  )
import System.Exit (exitFailure)

main :: IO ()
main = do
  counts <- runTestTT tests
  when (errors counts /= 0 || failures counts /= 0) exitFailure

tests :: Test
tests =
  TestList
  [ "calling a pure function" ~: piapprox @?= 4
  , "calling a non-pure function" ~: piapproxNonpure >>= (@?= 4)
  , "passing an argument" ~: do
    timesTwo 5 @?= 10
    timesTwo (-12) @?= -24
  ]
