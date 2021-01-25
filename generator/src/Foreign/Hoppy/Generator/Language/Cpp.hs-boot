-- This file is part of Hoppy.
--
-- Copyright 2015-2021 Bryan Gardiner <bog@khumba.net>
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

module Foreign.Hoppy.Generator.Language.Cpp (
  Generator,
  SayExportMode,
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT)
import {-# SOURCE #-} Foreign.Hoppy.Generator.Spec.Base (ErrorMsg)

type Generator = ReaderT Env (WriterT [Chunk] (Either ErrorMsg))

data Env

newtype Chunk = Chunk { chunkContents :: String }

data SayExportMode
