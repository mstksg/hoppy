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

-- | Bindings for @std@ that aren't in other modules.
module Foreign.Hoppy.Generator.Std (
  mod_std,
  c_string,
  ) where

import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Std.String (c_string)

{-# ANN module "HLint: ignore Use camelCase" #-}

-- | Include @std::string@.
mod_std :: Module
mod_std = modifyModule' (makeModule "std" "std.hpp" "std.cpp") $
  addModuleExports
  [ ExportClass c_string ]
