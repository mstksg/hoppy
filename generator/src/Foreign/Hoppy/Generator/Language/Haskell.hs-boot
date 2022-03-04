-- This file is part of Hoppy.
--
-- Copyright 2015-2022 Bryan Gardiner <bog@khumba.net>
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

{-# LANGUAGE CPP #-}

module Foreign.Hoppy.Generator.Language.Haskell (
  Managed,
  Generator,
  Output,
  SayExportMode,
  withErrorContext,
  addImports,
  sayLn,
  prettyPrint,
  ) where

import Control.Monad.Except (Except)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT)
import {-# SOURCE #-} Foreign.Hoppy.Generator.Spec.Base (HsImportSet)
import qualified Language.Haskell.Pretty as P

data Managed = Unmanaged | Managed

type Generator = ReaderT Env (WriterT Output (Except String))

data Env

data Output
instance Monoid Output

data SayExportMode

withErrorContext :: String -> Generator a -> Generator a

addImports :: HsImportSet -> Generator ()

sayLn :: String -> Generator ()

prettyPrint :: P.Pretty a => a -> String
