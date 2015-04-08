module Foreign.Cppop.Common (
  fromMaybeM,
  maybeFail,
  ) where

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM = flip maybe return

maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail = fromMaybeM . fail
