-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

import Control.Monad ((>=>), forM_)
import Foreign.Hoppy.Example.Instances
import Foreign.Hoppy.Example.Std
import Foreign.Hoppy.Runtime (delete, encodeAs, withCppObj)

main :: IO ()
main = do
  putStrLn "Creating a std::vector<std::string> and putting some things in it."
  v <- vectorString_new
  vectorString_pushBack v "Hello"
  vectorString_pushBack v "there"
  s <- encodeAs (undefined :: StdString) "joyful"
  vectorString_pushBack v s
  withCppObj "world!" $ \s' -> vectorString_pushBack v (s' :: StdStringConst)

  -- Walk through the vector using random access.
  size <- vectorString_size v
  putStrLn $ concat ["The vector now has ", show size, " elements.  They are:"]
  forM_ [0..size-1] $
    vectorString_at v >=> print
  delete v
