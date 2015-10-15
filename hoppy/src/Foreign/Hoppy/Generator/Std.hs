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
