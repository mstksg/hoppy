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

import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
  LocalBuildInfo,
  buildDir,
  )
import Distribution.Simple.Setup (
  configVerbosity,
  fromFlagOrDefault,
  )
import Distribution.Simple.UserHooks (UserHooks (postConf))
import Distribution.Simple.Utils (die')
import Distribution.Verbosity (Verbosity, normal)
import Foreign.Hoppy.Generator.Main (run)
import Foreign.Hoppy.Generator.Spec (
  ErrorMsg,
  Function,
  Interface,
  Module,
  Purity (Nonpure),
  addReqIncludes,
  ident,
  includeLocal,
  interface,
  interfaceAddHaskellModuleBase,
  makeFn,
  makeModule,
  moduleAddExports,
  moduleModify',
  moduleSetCppPath,
  moduleSetHppPath,
  toExport,
  )
import Foreign.Hoppy.Generator.Std (c_string, mod_std)
import Foreign.Hoppy.Generator.Types (objT)
import Foreign.Hoppy.Setup (
  GenerateLocation (GenerateInAutogenDir),
  ProjectConfig (..),
  combinedMain,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)

main :: IO ()
main = combinedMain project

project :: ProjectConfig
project = ProjectConfig
  { interfaceResult = ifaceResult
  , cppPackageName = ""  -- Must be null, not used with 'combinedMain'.
  , cppPackagedSourcesLocation = Just "cpp"
  , cppGeneratedSourcesLocation = GenerateInAutogenDir "cppgen"
  }

-- | The top level of the Hoppy interface definition for these bindings.
ifaceResult :: Either ErrorMsg Interface
ifaceResult =
  interfaceAddHaskellModuleBase ["Foreign", "Hoppy", "Examples", "OnePackage"] =<<
  interface "onepackageexample"
    [ mod_utils
    , moduleModify' mod_std $ do
        moduleSetHppPath "gen_std.hpp"
        moduleSetCppPath "gen_std.cpp"
    ]

-- | A module within the Hoppy interface, in which we specify the C++ entities
-- we want to wrap.
mod_utils :: Module
mod_utils =
  moduleModify' (makeModule "utils" "gen_utils.hpp" "gen_utils.cpp") $
  moduleAddExports [toExport f_reverse]

-- | The specification of the function we are wrapping.
f_reverse :: Function
f_reverse =
  addReqIncludes [includeLocal "utils.hpp"] $
  makeFn (ident "reverse") Nothing Nonpure [objT c_string] $ objT c_string
