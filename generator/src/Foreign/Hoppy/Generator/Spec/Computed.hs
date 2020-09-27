-- This file is part of Hoppy.
--
-- Copyright 2015-2020 Bryan Gardiner <bog@khumba.net>
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

-- | Module for computed data for an interface.
module Foreign.Hoppy.Generator.Spec.Computed (
  ComputedInterfaceData (..),
  EvaluatedEnumData (..),
  EvaluatedEnumValueMap,
  getEvaluatedEnumData,
  ) where

import qualified Data.Map as M
import Data.Map (Map)
import Foreign.Hoppy.Generator.Spec.Base (ExtName, Type)
import GHC.Stack (HasCallStack)

-- | Holds "computed data" for an interface.  This is data that is calculated by
-- Hoppy, beyond what is directly specified in the interface.
data ComputedInterfaceData = ComputedInterfaceData
  { computedInterfaceName :: String
    -- ^ The name of the interface.
  , evaluatedEnumMap :: Map ExtName EvaluatedEnumData
    -- ^ Evaluated numeric types and values for all enums in the interface.
  }

-- | Information about the enum that has been completed beyond what the
-- interface definition provides, possibly by building actual C++ code.
data EvaluatedEnumData = EvaluatedEnumData
  { evaluatedEnumType :: Type
    -- ^ The numeric type that C++ uses to hold the enum's values, or an
    -- equivalently-sized type.
  , evaluatedEnumValueMap :: EvaluatedEnumValueMap
    -- ^ Calculated values for all of the enum's entries.
  }

-- | Contains the numeric values for each of the entries in a C++ enum.
type EvaluatedEnumValueMap = Map [String] Integer

-- | Returns the map containing the calculated values for all entries in the
-- enum with the given 'ExtName'.  This requires hooks to have been run.
getEvaluatedEnumData ::
     HasCallStack
  => ComputedInterfaceData
  -> ExtName
  -> EvaluatedEnumData
getEvaluatedEnumData computed extName = case M.lookup extName (evaluatedEnumMap computed) of
  Nothing -> error $ "interfaceGetEvaluatedEnumData: No data found for " ++
             show extName ++ " in interface '" ++ computedInterfaceName computed ++ "'."
  Just info -> info
