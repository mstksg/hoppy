-- | A monad for consuming streams.
module Foreign.Cppop.Common.Consume (
  MonadConsume (..),
  ConsumeT,
  runConsumeT,
  evalConsumeT,
  execConsumeT,
  ) where

import Control.Monad (ap, liftM)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Tuple (swap)

-- | A typeclass for monads that can consume items from a stream.
class MonadConsume s m | m -> s where
  -- | Attempts to consume an item from the stream.  Returns an item if the
  -- stream is not empty.
  next :: m (Maybe s)

-- | A monad transformer for 'MonadConsume'.
newtype ConsumeT s m a = ConsumeT { getConsumeT :: StateT [s] m a }

instance Monad m => Functor (ConsumeT s m) where
  fmap = liftM

instance Monad m => Applicative (ConsumeT s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ConsumeT s m) where
  return = ConsumeT . return
  m >>= f = ConsumeT $ getConsumeT . f =<< getConsumeT m

instance MonadTrans (ConsumeT s) where
  lift = ConsumeT . lift

instance Monad m => MonadConsume s (ConsumeT s m) where
  next = do
    stream <- get'
    case stream of
      [] -> return Nothing
      x:xs -> put' xs >> return (Just x)

-- | Runs the consume action, returning the remainder of the stream, and the
-- action's result.
runConsumeT :: Monad m => [s] -> ConsumeT s m a -> m ([s], a)
runConsumeT stream (ConsumeT m) = liftM swap $ runStateT m stream

-- | Runs the consume action, returning the action's result.
evalConsumeT :: Monad m => [s] -> ConsumeT s m a -> m a
evalConsumeT stream = liftM snd . runConsumeT stream

-- | Runs the consume action, returning the remainder of the stream.
execConsumeT :: Monad m => [s] -> ConsumeT s m a -> m [s]
execConsumeT stream = liftM fst . runConsumeT stream

get' :: Monad m => ConsumeT s m [s]
get' = ConsumeT get

put' :: Monad m => [s] -> ConsumeT s m ()
put' = ConsumeT . put
