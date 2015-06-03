module Foreign.Cppop.Generator.Std (
  mod_std,
  c_string,
  ) where

import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std.List (c_list_int, c_list_string)
import Foreign.Cppop.Generator.Std.String (c_string)

{-# ANN module "HLint: ignore Use camelCase" #-}

mod_std :: Module
mod_std = modifyModule' (makeModule "std" "std.hpp" "std.cpp") $
  addModuleExports
  [ ExportClass c_list_int
  , ExportClass c_list_string
  , ExportClass c_string
  ]
