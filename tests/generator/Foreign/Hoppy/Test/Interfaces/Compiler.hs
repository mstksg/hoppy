-- This file is part of Hoppy.
--
-- Copyright 2015-2019 Bryan Gardiner <bog@khumba.net>
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

-- | C++ compiler specification for use with the test suite.
module Foreign.Hoppy.Test.Interfaces.Compiler (makeTestCompiler) where

import Foreign.Hoppy.Generator.Compiler (SimpleCompiler, defaultCompiler, prependArguments)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

-- | Creates a compiler that has a the C++ files for a particular test suite on
-- its include path.  We need this for enum value computation.
makeTestCompiler :: String -> SimpleCompiler
makeTestCompiler interfaceName =
  prependArguments ["-I" ++ cppDirPath </> interfaceName] defaultCompiler

-- | The path to the directory above each suite's C++ files (in subdirectories
-- named after the suites).
cppDirPath :: FilePath
{-# NOINLINE cppDirPath #-}
cppDirPath = unsafePerformIO $ getEnv "HOPPY_TEST_CPP_DIR"
