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

-- | Utilities for conditional compilation of parts of interfaces.
--
-- This module provides wrappers around 'Maybe' and 'catMaybes' so that you can
-- write code such as:
--
-- > myClass =
-- >   makeClass ...
-- >   [ ...ctors... ] $
-- >   collect
-- >   [ just $ mkMethod "foo" ...
-- >   , test (apiVersion >= [1, 2]) $ mkMethod "bar" ...
-- >   , test featureBaz $ mkMethod "baz" ...
-- >   ]
module Foreign.Hoppy.Generator.Version (
  -- * General filtering
  Filtered,
  collect,
  none,
  just,
  test,
  -- * C++ Standard version
  CppVersion (..),
  activeCppVersion,
  ) where

import Data.Maybe (catMaybes)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- | Placeholder 'Maybe'-like type that may be more general in the future.
type Filtered = Maybe

-- | Filters a list of 'Filtered' values down to the elements that are actually
-- present.
collect :: [Filtered a] -> [a]
collect = catMaybes

-- | Returns a 'Filtered' value that is always present.
just :: a -> Filtered a
just = Just

-- | Returns a 'Filtered' value that is only present if the boolean is true.
test :: Bool -> a -> Filtered a
test True = Just
test False = const Nothing

-- | Versions of the C++ standard.
data CppVersion =
    Cpp1998
  | Cpp2011
  | Cpp2014
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | The active version of the C++ standard.  This looks to the @HOPPY_CPP_STD@
-- environment variable, and accepts the values @c++98@, @c++11@, and @c++14@,
-- which map to the corresponding 'CppVersion' values.  If a value other than
-- these is set, then a warning is printed and the default ('Cpp2011') is used.
-- If no value is set, the default is used.
--
-- Note that this uses 'unsafePerformIO' internally and won't cope with a
-- changing environment.
activeCppVersion :: CppVersion
activeCppVersion = unsafePerformIO $ do
  str <- lookupEnv "HOPPY_CPP_STD"
  case str of
    Just "c++98" -> return Cpp1998
    Just "c++11" -> return Cpp2011
    Just "c++14" -> return Cpp2014
    Just _ -> do hPutStrLn stderr $ "Warning: Invalid HOPPY_CPP_STD value " ++ show str ++ "."
                 return def
    Nothing -> return def
  where def = Cpp2011
