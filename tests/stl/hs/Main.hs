{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (when)
import Foreign.Cppop.Runtime.Support (withScopedPtr)
import Foreign.Cppop.Test.Stl
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

tests :: Test
tests =
  TestList
  [ vectorTests
  ]

vectorTests :: Test
vectorTests =
  "std::vector" ~: TestList
  [ "basic operations" ~:
    withScopedPtr vectorString_new $ \v -> do
      vectorString_size v >>= (@?= 0)
      vectorString_empty v >>= (@?= True)
      vectorString_pushBack v "abc"
      vectorString_size v >>= (@?= 1)
      vectorString_empty v >>= (@?= False)
      vectorString_popBack v
      vectorString_size v >>= (@?= 0)
      vectorString_empty v >>= (@?= True)
  ]
