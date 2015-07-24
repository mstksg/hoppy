module Foreign.Cppop.Generator.Language.Haskell.General (
  Generator,
  Output,
  prettyPrint,
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer (WriterT)
import qualified Language.Haskell.Pretty as P

type Generator = ReaderT Env (WriterT Output (Either String))

data Env

data Output

prettyPrint :: P.Pretty a => a -> String
