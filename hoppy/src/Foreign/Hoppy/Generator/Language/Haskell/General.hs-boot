-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}

module Foreign.Hoppy.Generator.Language.Haskell.General (
  Generator,
  Output,
  prettyPrint,
  ) where

#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except (Except)
#endif
import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT)
import qualified Language.Haskell.Pretty as P

#if MIN_VERSION_mtl(2,2,1)
type Generator = ReaderT Env (WriterT Output (Except String))
#else
type Generator = ReaderT Env (WriterT Output (Either String))
#endif

data Env

data Output

prettyPrint :: P.Pretty a => a -> String
