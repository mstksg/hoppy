-- This file is part of Hoppy.
--
-- Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
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

module Foreign.Hoppy.Generator.Override (
  WithOverrides,
  plain,
  overridden,
  unoverriddenValue,
  overriddenValues,
  MapWithOverrides,
  plainMap,
  mapWithOverrides,
  addOverrideMap,
  addOverrideMaps,
  applyOverrideMaps,
  insertMapOverride,
  overriddenMapLookup,
  ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

-- | Represents a default value of type @v@ with optional overrides keyed by
-- parameter type @p@.  The type @p@ must have an 'Ord' instance.
data WithOverrides p v = WithOverrides
  { unoverriddenValue :: v
  , overriddenValues :: M.Map p v
  }

-- | Creates a 'WithOverrides' with the given default value @v@, and no
-- overridden values.
plain :: v -> WithOverrides p v
plain x = WithOverrides x M.empty

-- | Creates a 'WithOverrides' with the given default value @v@, and overridden
-- values in the map.
overridden :: v -> M.Map p v -> WithOverrides p v
overridden = WithOverrides

-- | Extracts a value, possibly overridden based on a parameter.
getOverride :: Ord p => p -> WithOverrides p v -> v
getOverride p o =
  fromMaybe (unoverriddenValue o) $ M.lookup p $ overriddenValues o

addOverride :: Ord p => p -> v -> WithOverrides p v -> WithOverrides p v
addOverride p v o = o { overriddenValues = M.insert p v $ overriddenValues o }

-- | Represents a map from @k@ values to @v@ values, where each entry can be
-- overridden based on parameter @p@.  A key is either present with a default
-- value and possibly some overridden values, or it is completely absent -- it
-- is not possible for a key to have overridden values but no default value.
newtype MapWithOverrides p k v =
  MapWithOverrides { fromMapWithOverrides :: M.Map k (WithOverrides p v) }

-- | Converts a plain map to a 'MapWithOverrides' without any overrides.
plainMap :: M.Map k v -> MapWithOverrides p k v
plainMap = MapWithOverrides . M.map plain

-- | Direct constructor for 'MapWithOverrides'.
mapWithOverrides :: M.Map k (WithOverrides p v) -> MapWithOverrides p k v
mapWithOverrides = MapWithOverrides

-- | Adds an override @v@ for key @k@ under parameter @p@ to a
-- 'MapWithOverrides'.
--
-- It is an error for a parameter to override a key that is not present in the
-- defaults map.
insertMapOverride ::
     (Ord p, Ord k, Show p, Show k)
  => p
  -> k
  -> v
  -> MapWithOverrides p k v
  -> MapWithOverrides p k v
insertMapOverride p k v (MapWithOverrides m) =
  -- We could do this whole operation as an 'alter' rather than a
  -- 'lookup'/'adjust', but this way, if an insertion is invalid then we return
  -- an error directly rather than hiding it inside the resulting structure.
  case M.lookup k m of
    Just _ -> MapWithOverrides $ M.adjust (addOverride p v) k m
    Nothing ->
      error $ "insertMapOverride: Can't add override for parameter " ++ show p ++
      " under key " ++ show k ++ " that has no default value."

-- | Adds a collection of overrides @v@ for multiple keys @k@, all under a single
-- parameter @p@, to a 'MapWithOverrides'.
--
-- It is an error for a parameter to override a key that is not present in the
-- defaults map.
addOverrideMap ::
     (Ord p, Ord k, Show p, Show k)
  => p
  -> M.Map k v
  -> MapWithOverrides p k v
  -> MapWithOverrides p k v
addOverrideMap p pOverrides (MapWithOverrides m) =
  MapWithOverrides $
  M.foldrWithKey (\k vOverride acc ->
                   M.alter (\kOverrides -> case kOverrides of
                             Just overrides ->
                               Just $ addOverride p vOverride overrides
                             Nothing ->
                               error $ "addOverrideMap: Parameter " ++ show p ++
                               " supplies override for key " ++ show k ++
                               " that is not in the map of unoverridden values.")
                           k
                           acc)
                 m
                 pOverrides

-- | Adds overrides @v@ for multiple keys @k@ under multiple parameters @p@ to a
-- 'MapWithOverrides'.
--
-- It is an error for a parameter to override a key that is not present in the
-- defaults map.
addOverrideMaps ::
     (Ord p, Ord k, Show p, Show k)
  => M.Map p (M.Map k v)
  -> MapWithOverrides p k v
  -> MapWithOverrides p k v
addOverrideMaps overrideMaps (MapWithOverrides m) =
  MapWithOverrides $
  M.foldrWithKey (\p pOverrides ->
                   fromMapWithOverrides . addOverrideMap p pOverrides . MapWithOverrides)
                 m
                 overrideMaps

-- | Constructs a 'MapWithOverrides' from a map of default values and a bunch of
-- parameter-specific maps overlaid on top of it.
--
-- It is an error for a parameter to override a key that is not present in the
-- defaults map.
applyOverrideMaps ::
     (Ord p, Ord k, Show p, Show k)
  => M.Map p (M.Map k v)
  -> M.Map k v
  -> MapWithOverrides p k v
applyOverrideMaps overrideMaps baseMap =
  MapWithOverrides $
  M.foldrWithKey (\p pOverrides acc ->
                   M.foldrWithKey (\k vOverride acc' ->
                                    M.alter (\kOverrides -> case kOverrides of
                                              Just overrides ->
                                                Just $ addOverride p vOverride overrides
                                              Nothing ->
                                                error $ "applyOverrideMaps: Parameter " ++ show p ++
                                                " supplies override for key " ++ show k ++
                                                " that is not in the map of unoverridden values.")
                                            k
                                            acc')
                                  acc
                                  pOverrides)
                 (M.map plain baseMap)
                 overrideMaps

-- | Looks up a value for @k@ in the given 'MapWithOverrides', with the
-- possibility that the value is overridden by the parameter @p@.
overriddenMapLookup :: (Ord p, Ord k) => p -> k -> MapWithOverrides p k v -> Maybe v
overriddenMapLookup p k (MapWithOverrides x) = getOverride p <$> M.lookup k x
