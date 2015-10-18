-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (when)
import Foreign.Hoppy.Runtime.Support (withScopedPtr)
import Foreign.Hoppy.Test.Stl
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
