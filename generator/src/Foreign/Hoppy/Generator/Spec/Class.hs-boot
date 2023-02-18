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

module Foreign.Hoppy.Generator.Spec.Class (
  Class,
  classExtName,
  classIdentifier,
  classReqs,
  classConversion,
  ClassConversion (..),
  ClassHaskellConversion (..),
  toHsValueClassName,
  toHsWithValuePtrName,
  toHsPtrClassName,
  toHsCastMethodName,
  toHsDataTypeName,
  toHsDataCtorName,
  ) where

import {-# SOURCE #-} Foreign.Hoppy.Generator.Spec.Base (Constness, ExtName, Identifier, Reqs)
import {-# SOURCE #-} qualified Foreign.Hoppy.Generator.Language.Haskell as LH
import Language.Haskell.Syntax (HsType)

data Class

instance Eq Class
instance Ord Class
instance Show Class

classExtName :: Class -> ExtName

classIdentifier :: Class -> Identifier

classReqs :: Class -> Reqs

classConversion :: Class -> ClassConversion

data ClassConversion = ClassConversion
  { classHaskellConversion :: ClassHaskellConversion
  }

data ClassHaskellConversion = ClassHaskellConversion
  { classHaskellConversionType :: Maybe (LH.Generator HsType)
  , classHaskellConversionToCppFn :: Maybe (LH.Generator ())
  , classHaskellConversionFromCppFn :: Maybe (LH.Generator ())
  }

toHsValueClassName :: Class -> LH.Generator String

toHsWithValuePtrName :: Class -> LH.Generator String

toHsPtrClassName :: Constness -> Class -> LH.Generator String

toHsCastMethodName :: Constness -> Class -> LH.Generator String

toHsDataTypeName :: Constness -> Class -> LH.Generator String

toHsDataCtorName :: LH.Managed -> Constness -> Class -> LH.Generator String
