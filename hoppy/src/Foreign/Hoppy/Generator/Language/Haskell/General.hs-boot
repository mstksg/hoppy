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
