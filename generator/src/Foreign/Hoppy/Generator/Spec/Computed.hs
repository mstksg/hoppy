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

-- | Module for computed data for an interface.
module Foreign.Hoppy.Generator.Spec.Computed (
  ComputedInterfaceData (..),
  EvaluatedEnumData (..),
  EvaluatedEnumValueMap,
  getEvaluatedEnumData,
  -- * Numeric types
  NumericTypeInfo,
  numType, numBytes, numMinBound, numMaxBound,
  findNumericTypeInfo,
  pickNumericType,
  ) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Foreign.C (CInt, CLong, CLLong, CUInt, CULong, CULLong)
import Foreign.Hoppy.Generator.Spec.Base (ExtName, Type)
import Foreign.Hoppy.Generator.Types (intT, llongT, longT, uintT, ullongT, ulongT)
import Foreign.Storable (Storable, sizeOf)
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
  { evaluatedEnumNumericType :: NumericTypeInfo
    -- ^ The numeric type that C++ uses to hold the enum's values, or an
    -- equivalently-sized type.
  , evaluatedEnumValueMap :: EvaluatedEnumValueMap
    -- ^ Calculated values for all of the enum's entries.
  } deriving (Read, Show)

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

-- | Bound information about numeric types.
data NumericTypeInfo = NumericTypeInfo
  { numType :: Type
    -- ^ The numeric data type described by the record.
  , numBytes :: Int
    -- ^ The number of bytes in a value of the type.
  , numMinBound :: Integer
    -- ^ The lowest (most negative) value representable by the type.
  , numMaxBound :: Integer
    -- ^ The highest (most positive) value representable by the type.
  }

instance Show NumericTypeInfo where
  show info = show (numBytes info, numMinBound info, numMaxBound info)

instance Read NumericTypeInfo where
  readsPrec p s =
    case readsPrec p s of
      [((bytes, minBound, maxBound), rest)] ->
        case pickNumericType bytes minBound maxBound of
          Just info -> [(info, rest)]
          Nothing -> []
      [] -> []
      other ->
        error $ "Read NumericTypeInfo: Unexpected readsPrec result: " ++ show other

-- | Numeric types usable to hold enum values.  These are ordered by decreasing
-- precedence (increasing word size).
numericTypeInfo :: [NumericTypeInfo]
numericTypeInfo =
  [ mk intT (undefined :: CInt)
  , mk uintT (undefined :: CUInt)
  , mk longT (undefined :: CLong)
  , mk ulongT (undefined :: CULong)
  , mk llongT (undefined :: CLLong)
  , mk ullongT (undefined :: CULLong)
  ]
  where mk :: forall a. (Bounded a, Integral a, Storable a) => Type -> a -> NumericTypeInfo
        mk t _ = NumericTypeInfo
                 { numType = t
                 , numBytes = sizeOf (undefined :: a)
                 , numMinBound = toInteger (minBound :: a)
                 , numMaxBound = toInteger (maxBound :: a)
                 }

-- | Searches the list of known numeric types usable for enum values, and
-- returns the record for the given type.
findNumericTypeInfo :: Type -> Maybe NumericTypeInfo
findNumericTypeInfo t = listToMaybe $ filter (\i -> numType i == t) numericTypeInfo

-- | Selects the preferred numeric type for holding numeric values in the given
-- range.
pickNumericType :: Int -> Integer -> Integer -> Maybe NumericTypeInfo
pickNumericType bytes low high =
  listToMaybe $ flip filter numericTypeInfo $ \info ->
  numBytes info == bytes &&
  numMinBound info <= low &&
  numMaxBound info >= high
