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

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad ((>=>), forM_, when)
import Foreign (peek)
import Foreign.C (castCCharToChar, withCString, withCStringLen)
import Foreign.Hoppy.Runtime (
  HasContents, assign, decode, encode, fromContents, toContents, toGc, withCppObj, withScopedPtr)
import Foreign.Hoppy.Test.Std
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

assertList :: (HasContents l a, Eq a, Show a) => [a] -> l -> IO ()
assertList expectedContents list = toContents list >>= (@?= expectedContents)

tests :: Test
tests =
  TestList
  [ stringTests
  , "containers" ~: TestList
    [ listIntTests
    , vectorTests
    ]
  ]

stringTests :: Test
stringTests =
  "strings" ~: TestList
  [ "encoding" ~:
    withScopedPtr (encode "Hello, world!" :: IO StdString) $ \p -> do
    stdString_size p >>= (@?= 13)
    forM_ (zip [0..] "Hello, world!") $ \(i, c) ->
      stdString_get p i >>= (@?= c) . castCCharToChar

  , "encoding NUL" ~:
    withScopedPtr (encode "A\NULB" :: IO StdString) $ \p -> do
    stdString_size p >>= (@?= 3)
    forM_ (zip [0..] "A\NULB") $ \(i, c) ->
      stdString_get p i >>= (@?= c) . castCCharToChar

  , "decoding" ~:
    withCString "Hello, world!" $ \cs ->
    withScopedPtr (stdString_newFromCString cs) $ \s ->
    decode s >>= (@?= "Hello, world!")

  , "decoding NUL" ~:
    withCStringLen "A\NULB" $ \cs ->
    withScopedPtr (stdString_newFromCStringLen cs) $ \s ->
    decode s >>= (@?= "A\NULB")

  , "assignment" ~:
    withCppObj "Hello" $ \(s :: StdString) -> do
    assign s "planet!"
    decode s >>= (@?= "planet!")
  ]

listIntTests :: Test
listIntTests =
  "std::list<int>" ~: TestList
  [ "creation" ~: withScopedPtr listInt_new $ listInt_empty >=> (@?= True)

  , "contents conversion" ~: do
    let input = [1, 2, 4, 8]
    l <- toGc =<< fromContents input :: IO ListInt
    listInt_size l >>= (@?= 4)
    toContents l >>= (@?= input)

  , "pushing, popping, clearing" ~: withScopedPtr listInt_new $ \l -> do
    listInt_pushFront l 1
    listInt_pushBack l 2
    listInt_pushFront l 3
    listInt_pushBack l 4
    assertList [3, 1, 2, 4] l
    listInt_popFront l
    assertList [1, 2, 4] l
    listInt_popBack l
    assertList [1, 2] l
    listInt_clear l
    assertList [] l

  , "iterating" ~: do
    l <- toGc =<< fromContents [1, 5] :: IO ListInt
    end <- listInt_end l
    it <- listInt_begin l
    listIntIterator_get it >>= peek >>= (@?= 1)
    listIntIterator_put it 2
    _ <- listIntIterator_next it
    listIntIterator_get it >>= peek >>= (@?= 5)
    listIntIterator_EQ it end >>= (@?= False)
    _ <- listIntIterator_next it
    listIntIterator_EQ it end >>= (@?= True)
    listInt_front l >>= peek >>= (@?= 2)
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
