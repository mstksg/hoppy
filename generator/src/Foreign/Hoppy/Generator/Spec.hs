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

-- | The primary data types for specifying C++ interfaces.
--
-- 'Show' instances in this module produce strings of the form @\"\<TypeOfObject
-- nameOfObject otherInfo...\>\"@.  They can be used in error messages without
-- specifying a noun separately, i.e. write @show cls@ instead of @\"the class
-- \" ++ show cls@.
module Foreign.Hoppy.Generator.Spec (
  module Foreign.Hoppy.Generator.Spec.Base,
  module Foreign.Hoppy.Generator.Spec.Callback,
  module Foreign.Hoppy.Generator.Spec.Class,
  module Foreign.Hoppy.Generator.Spec.ClassFeature,
  module Foreign.Hoppy.Generator.Spec.Computed,
  module Foreign.Hoppy.Generator.Spec.Conversion,
  module Foreign.Hoppy.Generator.Spec.Enum,
  module Foreign.Hoppy.Generator.Spec.Function,
  module Foreign.Hoppy.Generator.Spec.Variable,
  ) where

import Foreign.Hoppy.Generator.Spec.Base
import Foreign.Hoppy.Generator.Spec.Callback
import Foreign.Hoppy.Generator.Spec.Class
import Foreign.Hoppy.Generator.Spec.ClassFeature
import Foreign.Hoppy.Generator.Spec.Computed
import Foreign.Hoppy.Generator.Spec.Conversion
import Foreign.Hoppy.Generator.Spec.Enum
import Foreign.Hoppy.Generator.Spec.Function
import Foreign.Hoppy.Generator.Spec.Variable
