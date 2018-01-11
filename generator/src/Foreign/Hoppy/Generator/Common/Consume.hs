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

{-# LANGUAGE CPP #-}

-- | A monad for consuming streams.
module Foreign.Hoppy.Generator.Common.Consume (
  MonadConsume (..),
  ConsumeT,
  runConsumeT,
  evalConsumeT,
  execConsumeT,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), Applicative, pure)
#endif
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
