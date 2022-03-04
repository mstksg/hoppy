-- This file is part of Hoppy.
--
-- Copyright 2015-2022 Bryan Gardiner <bog@khumba.net>
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

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (when)
import Foreign.Hoppy.Runtime (fromCppEnum)
import Foreign.Hoppy.Test.Enumeval
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
  [ iceCreamTests
  , numberTests
  , rankIceCreamTests
  ]

iceCreamTests :: Test
iceCreamTests =
  "IceCream enum" ~: TestList
  [ "chocolate" ~: fromCppEnum IceCream_Chocolate @?= 1
  , "bubblegum" ~: fromCppEnum IceCream_Bubblegum @?= 2
  , "birthday cake" ~: fromCppEnum IceCream_BirthdayCake @?= 3
  ]

numberTests :: Test
numberTests =
  "Number enum" ~: TestList
  [ "one" ~: fromCppEnum Number_One @?= 0
  , "oneAndAHalf" ~: fromCppEnum Number_OneAndAHalf @?= 1
  , "Two" ~: fromCppEnum Number_Two @?= 2
  , "THREE" ~: fromCppEnum Number_Three @?= 3
  , "four_fiveSixSEVEN" ~: fromCppEnum Number_FourFiveSixSeven @?= 4
  ]

rankIceCreamTests :: Test
rankIceCreamTests =
  "rankIceCream function" ~: TestList
  [ "chocolate" ~: rankIceCream IceCream_Chocolate @?= 1000
  , "bubblegum" ~: rankIceCream IceCream_Bubblegum @?= 45
  , "birthday cake" ~: rankIceCream IceCream_BirthdayCake @?= 80
  ]
