-- This file is part of Hoppy.
--
-- Copyright 2015-2019 Bryan Gardiner <bog@khumba.net>
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

module Foreign.Hoppy.Generator.Spec.Variable where
-- TODO Docs and export list.

import Data.Function (on)
import Foreign.Hoppy.Generator.Spec.Base
import qualified Foreign.Hoppy.Generator.Spec.Class as Class
import qualified Foreign.Hoppy.Generator.Language.Cpp as LC
import qualified Foreign.Hoppy.Generator.Language.Haskell as LH

-- | A C++ variable.
--
-- Use this data type's 'HasReqs' instance to make the variable accessible.
data Variable = Variable
  { varIdentifier :: Identifier
    -- ^ The identifier used to refer to the variable.
  , varExtName :: ExtName
    -- ^ The variable's external name.
  , varType :: Type
    -- ^ The variable's type.  This may be
    -- 'Foreign.Hoppy.Generator.Types.constT' to indicate that the variable is
    -- read-only.
  , varReqs :: Reqs
    -- ^ Requirements for bindings to access this variable.
  , varAddendum :: Addendum
    -- ^ The variable's addendum.
  }

instance Eq Variable where
  (==) = (==) `on` varExtName

instance Show Variable where
  show v = concat ["<Variable ", show (varExtName v), " ", show (varType v), ">"]

instance Exportable Variable where
  sayExportCpp = sayCppExport
  sayExportHaskell = sayHsExport

instance HasExtNames Variable where
  getPrimaryExtName = varExtName
  getNestedExtNames v = [varGetterExtName v, varSetterExtName v]

instance HasReqs Variable where
  getReqs = varReqs
  setReqs reqs v = v { varReqs = reqs }

instance HasAddendum Variable where
  getAddendum = varAddendum
  setAddendum addendum v = v { varAddendum = addendum }

-- | Creates a binding for a C++ variable.
makeVariable :: Identifier -> Maybe ExtName -> Type -> Variable
makeVariable identifier maybeExtName t =
  Variable identifier (extNameOrIdentifier identifier maybeExtName) t mempty mempty

-- | Returns whether the variable is constant, i.e. whether its type is
-- @'Foreign.Hoppy.Generator.Types.constT' ...@.
varIsConst :: Variable -> Bool
varIsConst v = case varType v of
  Internal_TConst _ -> True
  _ -> False

-- | Returns the external name of the getter function for the variable.
varGetterExtName :: Variable -> ExtName
varGetterExtName = toExtName . (++ "_get") . fromExtName . varExtName

-- | Returns the external name of the setter function for the variable.
varSetterExtName :: Variable -> ExtName
varSetterExtName = toExtName . (++ "_set") . fromExtName . varExtName

sayCppExport :: LC.SayExportMode -> Variable -> LC.Generator ()
sayCppExport mode v = case mode of
  LC.SayHeader -> return ()
  LC.SaySource ->
    Class.sayCppExportVar (varType v)
                          Nothing
                          True
                          (varGetterExtName v)
                          (varSetterExtName v)
                          (LC.sayIdentifier $ varIdentifier v)

sayHsExport :: LH.SayExportMode -> Variable -> LH.Generator ()
sayHsExport mode v = LH.withErrorContext ("generating variable " ++ show (varExtName v)) $ do
  let getterName = varGetterExtName v
      setterName = varSetterExtName v
  Class.sayHsExportVar mode
                       (varType v)
                       Nothing
                       True
                       getterName
                       getterName
                       setterName
                       setterName
