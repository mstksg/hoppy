module Foreign.Cppop.Runtime.IdSpace (
  IdSpace (..),
  ListIdSpace,
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)

class IdSpace s n | s -> n where
  create :: String -> n -> n -> IO s

  name :: s -> String

  request :: s -> IO n

  release :: s -> n -> IO ()

data ListIdSpace n = ListIdSpace
  { listIdSpaceName :: String
  , listIdSpaceBase :: n
  , listIdSpaceStep :: n
  , listIdSpaceVar :: MVar [n]
  }

instance (Eq n, Num n, Show n) => IdSpace (ListIdSpace n) n where
  create name base step = ListIdSpace name base step <$> newMVar []

  name = listIdSpaceName

  request space = modifyMVar (listIdSpaceVar space) $ return . \list ->
    let list'@(n':_) = case list of
          [] -> [listIdSpaceBase space]
          n:_ -> n + listIdSpaceStep space : list
    in (list', n')

  release space n = modifyMVar_ (listIdSpaceVar space) $ return . \list ->
    case tryDelete n list of
      Just list' -> list'
      Nothing -> fail $ "Couldn't release id " ++ show n ++ " from space \"" ++
                 show (name space) ++ "\"; id not in use."

tryDelete :: Eq a => a -> [a] -> Maybe [a]
tryDelete x (y:ys) | x == y = Just ys
                   | otherwise = (y:) <$> tryDelete x ys
tryDelete _ [] = Nothing
