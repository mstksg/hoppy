module Foreign.Cppop.Common (
  fromMaybeM,
  maybeFail,
  ) where

-- | @fromMaybeM m x = maybe m return x@
fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM = flip maybe return

-- | @maybeFail s x = maybe (fail s) x@
maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail = fromMaybeM . fail
