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

import Data.Monoid (mempty)
import Foreign.Hoppy.Generator.Main (run)
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.Template
import Foreign.Hoppy.Generator.Std (mod_std)
import Foreign.Hoppy.Generator.Std.String (c_string)
import Foreign.Hoppy.Generator.Std.Vector (tc_vector)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- | Our @main@ (a) ensures that creating an 'Interface' didn't cause any
-- errors, and (b) calls the Hoppy generator main function, 'run'.
main :: IO ()
main = case interfaceResult of
  Left errorMsg -> do
    putStrLn $ "Error initializing interface: " ++ errorMsg
    exitFailure
  Right iface -> do
    args <- getArgs
    run [iface] args
    return ()

-- | An interface that contains two modules (the @std@ one provided by Hoppy,
-- and a custom module that instantiates some templates), and will generate
-- Haskell code in @Foreign.Hoppy.Example@.
interfaceResult :: Either String Interface
interfaceResult =
  addInterfaceHaskellModuleBase ["Foreign", "Hoppy", "Example"] =<<
  interface "example" modules

modules :: [Module]
modules = [mod_std, instancesModule]

instancesModule :: Module
instancesModule =
  modifyModule' (makeModule "instances" "instances.hpp" "instances.cpp") $
  addModuleExports
  [ ExportClass c_vector_string
  ]

-- | An instantiation of the @std::vector<>@ template, with @std::string@ as its
-- argument.  This class's name is @vectorString@, a concatenation of the
-- template's name and the suffix given here.
c_vector_string :: Class
c_vector_string = instantiateClassTemplate' tc_vector "String" [TObj c_string] [] mempty
