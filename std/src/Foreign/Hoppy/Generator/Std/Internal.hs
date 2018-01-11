-- This file is part of Hoppy.
--
-- Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- | Package-specific utilities.
module Foreign.Hoppy.Generator.Std.Internal (
  includeHelper,
  ) where

import Foreign.Hoppy.Generator.Spec (Include, includeLocal)
import Paths_hoppy_std (getDataFileName)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))

includeDir :: FilePath
{-# NOINLINE includeDir #-}
includeDir = unsafePerformIO $ getDataFileName $ "include" </> "hoppy" </> "std"

-- | Returns a C++ include pointing to the header specified, within the
-- installed location corresponding to the @include/hoppy/std@ path in the @std@
-- sources.  (Example: @includeHelper \"utility.hpp\"@)
includeHelper :: FilePath -> Include
includeHelper filename = includeLocal $ includeDir </> filename
