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

{-# LANGUAGE CPP #-}

-- | General routines.
module Foreign.Hoppy.Common (
  fromMaybeM,
  fromEitherM,
  maybeFail,
  listSubst,
  zipWithM,
  writeFileIfDifferent,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception (evaluate)
import Control.Monad (when)
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
