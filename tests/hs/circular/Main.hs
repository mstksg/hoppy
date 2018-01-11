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

module Main (main) where

import Control.Monad (when)
import Data.Bits ((.|.))
import Foreign.C (castCCharToChar, castCharToCChar)
import Foreign.Hoppy.Runtime (nullptr, toGc, toPtr)
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

    flobObj <- toGc =<< flobClass_new
    flubObj <- toGc =<< flubClass_new
    takesFlobValues flobObj
    takesFlubValues flubObj FlubEnum_OptionA flubBitspace_OptionB
    flubClass_flubClassVar_set flubObj 11
    flubClass_flubStaticClassVar_set 22
    flubClass_flubClassVar_get (toFlubClassConst flubObj) >>= (@?= 11)
    flubClass_flubStaticClassVar_get >>= (@?= 22)
    flubClass_flubStaticConstClassVar_get >>= (@?= 33)

    fmap toPtr returnsFlubClass >>= (@?= toPtr nullptr)
    returnsFlubEnum >>= (@?= FlubEnum_OptionB)
    returnsFlubBitspace >>= (@?= flubBitspace_OptionA .|. flubBitspace_OptionC)
  ]
