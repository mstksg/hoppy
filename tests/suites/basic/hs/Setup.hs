-- This file is part of Hoppy.
--
-- Copyright 2015-2023 Bryan Gardiner <bog@khumba.net>
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

import Foreign.Hoppy.Setup (
  ProjectConfig (..),
  GenerateLocation (GenerateInAutogenDir),
  hsMain,
  )
import qualified Foreign.Hoppy.Test.Interfaces.Basic as Basic

main =
  hsMain
  ProjectConfig
  { interfaceResult = Basic.interfaceResult
  , cppPackageName = "hoppy-tests-basic-cpp"
  , cppPackagedSourcesLocation = Just "cpp"  -- tests/suites/basic/cpp/cpp
  , cppGeneratedSourcesLocation = GenerateInAutogenDir "cpp"  -- .../autogen/cpp
  }
