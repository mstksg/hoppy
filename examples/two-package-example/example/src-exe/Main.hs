-- This file is part of Hoppy.
--
-- Copyright 2015-2024 Bryan Gardiner <bog@khumba.net>
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

module Main (main) where

-- The Std import is needed to bring in "instance StdStringValue String".
import Foreign.Hoppy.Examples.TwoPackage.Std ()
import Foreign.Hoppy.Examples.TwoPackage.Utils (reverse)
import Prelude hiding (reverse)

main :: IO ()
main = putStr . unlines =<< mapM reverse . lines =<< getContents
