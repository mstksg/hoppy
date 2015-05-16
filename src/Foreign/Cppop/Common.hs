module Foreign.Cppop.Common (
  fromMaybeM,
  maybeFail,
  listSubst,
  zipWithM,
  writeFileIfDifferent,
  ) where

import Control.Applicative ((<$>))
import Control.Exception (bracket, evaluate)
import Control.Monad (when)
import System.Directory (doesFileExist)
import System.IO (
  IOMode (ReadMode),
  hClose,
  hGetContents,
  openFile,
  )

-- | @fromMaybeM m x = maybe m return x@
fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM = flip maybe return

-- | @maybeFail s x = maybe (fail s) x@
maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail = fromMaybeM . fail

listSubst :: Eq a => a -> a -> [a] -> [a]
listSubst x x' = map $ \y -> if y == x then x' else y

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
  where readStrictly = bracket (openFile path ReadMode) hClose $ \handle -> do
            contents <- hGetContents handle
            _ <- evaluate $ length contents
            return contents
