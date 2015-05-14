module Foreign.Cppop.Common (
  fromMaybeM,
  maybeFail,
  listSubst,
  zipWithM,
  ) where

-- | @fromMaybeM m x = maybe m return x@
fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM = flip maybe return

-- | @maybeFail s x = maybe (fail s) x@
maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail = fromMaybeM . fail

listSubst :: Eq a => a -> a -> [a] -> [a]
listSubst x x' = map $ \y -> if y == x then x' else y

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence $ zipWith f xs ys
