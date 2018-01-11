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

{-# LANGUAGE CPP #-}

-- | General routines.
module Foreign.Hoppy.Generator.Common (
  fromMaybeM,
  fromEitherM,
  maybeFail,
  for,
  listSubst,
  zipWithM,
  writeFileIfDifferent,
  -- * String utilities
  capitalize,
  lowerFirst,
  upperFirst,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Char (toLower, toUpper)
import System.Directory (doesFileExist)
import System.IO (IOMode (ReadMode), hGetContents, withFile)

-- | @fromMaybeM m x = maybe m return x@
fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM = flip maybe return

-- | @fromEitherM f x = either f return x@
fromEitherM :: Monad m => (e -> m a) -> Either e a -> m a
fromEitherM = flip either return

-- | @maybeFail s x = maybe (fail s) x@
maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail = fromMaybeM . fail

-- | @for = flip map@
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | @listSubst a b xs@ replaces all @x@ in @xs@ such that @x == a@ with @b@.
listSubst :: Eq a => a -> a -> [a] -> [a]
listSubst x x' = map $ \y -> if y == x then x' else y

-- | Zips two lists using a monadic function.
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence $ zipWith f xs ys

-- | If the file specified does not exist or its contents does not match the
-- given string, then this writes the string to the file.
writeFileIfDifferent :: FilePath -> String -> IO ()
writeFileIfDifferent path newContents = do
  exists <- doesFileExist path
  -- We need to read the file strictly, otherwise lazy IO might try to write the
  -- file while it's still open and locked for reading.
  doWrite <- if exists
             then (newContents /=) <$> readStrictly
             else return True
  when doWrite $ writeFile path newContents
  where readStrictly = withFile path ReadMode $ \handle -> do
            contents <- hGetContents handle
            _ <- evaluate $ length contents
            return contents

-- | Upper cases the first character of a string, and lower cases the rest of
-- it.  Does nothing to an empty string.
capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : map toLower cs

-- | Lower cases the first character of a string, if nonempty.
lowerFirst :: String -> String
lowerFirst "" = ""
lowerFirst (c:cs) = toLower c : cs

-- | Upper cases the first character of a string, if nonempty.
upperFirst :: String -> String
upperFirst "" = ""
upperFirst (c:cs) = toUpper c : cs
