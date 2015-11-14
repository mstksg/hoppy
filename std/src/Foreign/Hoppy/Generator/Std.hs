-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

-- | Bindings for @std@ that aren't in other modules.
module Foreign.Hoppy.Generator.Std (
  ValueConversion (..),
  mod_std,
  c_string,
  ) where

import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Std.String (c_string)

{-# ANN module "HLint: ignore Use camelCase" #-}

-- | Specifies how values in a collection should be converted when converting
-- the collection as a whole.
data ValueConversion =
    ConvertPtr
    -- ^ A C++ value of type @t@ will convert to a foreign value of type @'TPtr'
    -- t@.
  | ConvertValue
    -- ^ A C++ value of type @t@ will convert to a foreign value of type @t@.
    -- For an object type ('TObj'), the class must have conversions
    -- ('ClassConversion').
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Include @std::string@.
mod_std :: Module
mod_std = modifyModule' (makeModule "std" "std.hpp" "std.cpp") $
  addModuleExports
  [ ExportClass c_string ]
