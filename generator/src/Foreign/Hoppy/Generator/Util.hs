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

-- | Miscellaneous utilities that may be useful in Hoppy generators.
module Foreign.Hoppy.Generator.Util (
  -- * String utilities
  splitIntoWords,
  -- * File utilities
  withTempFile,
  withTempDirectory,
  ) where

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (when)
import Data.Char (isDigit, isLetter, isLower, isUpper)
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  getTemporaryDirectory,
  removeFile,
  removeDirectoryRecursive,
  )
import System.IO (Handle, openTempFile)
import System.IO.Temp (createTempDirectory)

-- | Splits a C++ identifier string into multiple words, doing smart inspection
-- of the case convention of the string.  This supports @snake_case@ and
-- @CONSTANT_CASE@, and recognition of @camelCase@, including when acronyms are
-- uppercased (@\"HTMLElement\"@ gives @[\"HTML\", \"Element\"]@).  Numbers are
-- treated as their own words, and non-alphanumeric characters are treated as
-- word separators and dropped.
splitIntoWords :: String -> [String]
splitIntoWords cs = case cs of
  "" -> []

  -- The case of multiple upper-case letters, e.g. "HTML", or "HTMLElement".
  c1:c2:_ | isUpper c1 && isUpper c2 ->
    let (upperWord, rest) = span isUpper cs
        (upperWord', rest') = case rest of
          -- This handles the "HTMLElement" case, where we need to shift from
          -- "HTMLE"/"lement" to "HTML"/"Element".
          c3:_ | isLower c3 ->
            let (word, lastChar) = splitAt (length upperWord - 1) upperWord
            in (word, lastChar ++ rest)
          -- But if the part after the upper case part doesn't start with a
          -- lower case letter, then there's no rearranging to do.
          _ -> (upperWord, rest)
    in upperWord' : splitIntoWords rest'

  -- The case of letters, but not multiple upper case letters.  Here we want to
  -- take grab "foo" from "fooBar", "a" from "aWidget", and "Too" from "TooNie".
  c1:cs' | isLetter c1 ->
    let (wordTail, rest) = span isLower cs'
    in (c1:wordTail) : splitIntoWords rest

  -- Numbers get treated as their own words.
  c1:_ | isDigit c1 ->
    let (word, rest) = span isDigit cs
    in word : splitIntoWords rest

  -- Non-alphanumeric characters may act as word barriers, but otherwise get
  -- dropped.
  _:cs' -> splitIntoWords cs'

-- | Creates a temporary file whose name is based on the given template string,
-- and runs the given function with the path to the file.  The file is deleted
-- when the function completes, if the boolean that the function returns (or, in
-- case of an exception, the boolean that was passed directly to 'withTempFile')
-- is true.
withTempFile :: String -> Bool -> (FilePath -> Handle -> IO (Bool, a)) -> IO a
withTempFile template deleteOnException f = do
  tempDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tempDir template
  catch (do (delete, result) <- f path handle
            when delete $ removeFileIfExists path
            return result)
        (\(e :: IOException) -> do
           when deleteOnException $ removeFileIfExists path
           throwIO e)
  where removeFileIfExists path = do
          exists <- doesFileExist path
          when exists $ removeFile path

-- | Creates a temporary directory whose name is based on the given template
-- string, and runs the given function with the directory's path.  The directory
-- is deleted when the function completes, if the boolean that the function
-- returns (or, in case of an exception, the boolean that was passed directly to
-- 'withTempDirectory') is true.
withTempDirectory :: String -> Bool -> (FilePath -> IO (Bool, a)) -> IO a
withTempDirectory template deleteOnException f = do
  outerDir <- getTemporaryDirectory
  dir <- createTempDirectory outerDir template
  catch (do (delete, result) <- f dir
            when delete $ removeIfExists dir
            return result)
        (\(e :: IOException) -> do
           when deleteOnException $ removeIfExists dir
           throwIO e)
  where removeIfExists dir = do
          exists <- doesDirectoryExist dir
          when exists $ removeDirectoryRecursive dir
