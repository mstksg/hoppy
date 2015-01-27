module Foreign.Cppop.Runtime.SeqNum (
  SeqNum,
  SeqNums,
  new,
  grabBriefly,
  grabLong,
  release,
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Bits (testBit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CInt)

type SeqNum = CInt

data SeqNums = SeqNums
  { briefsRef :: IORef [SeqNum]  -- Odd numbers only, strictly decreasing.
  , longsRef :: IORef [SeqNum]  -- Even numbers only, strictly decreasing.
  }

data Type = Brief | Long

getType :: SeqNum -> Type
getType num =
  let isOdd = testBit num 0
  in if isOdd then Brief else Long

getRef :: SeqNums -> SeqNum -> IORef [SeqNum]
getRef nums num = case getType num of
  Brief -> briefsRef nums
  Long -> longsRef nums

new :: IO SeqNums
new = SeqNums <$> newIORef [] <*> newIORef []

grabBriefly :: SeqNums -> IO SeqNum
grabBriefly nums = modifyRef (briefsRef nums) $ \xs -> case xs of
  [] -> ([1], 1)
  x:_ -> let x' = x + 2 in (x':xs, x')

grabLong :: SeqNums -> IO SeqNum
grabLong nums = modifyRef (longsRef nums) $ \xs -> case xs of
  [] -> ([2], 2)
  x:_ -> let x' = x + 2 in (x':xs, x')

release :: SeqNums -> SeqNum -> IO ()
release nums num = do
  let ref = getRef nums num
  inUse <- readIORef ref
  case tryDelete num inUse of
    Nothing -> fail $ "SeqNum.release: Sequence number " ++ show num ++ " is not in use."
    Just inUse' -> writeIORef ref inUse'

tryDelete :: Eq a => a -> [a] -> Maybe [a]
tryDelete x (y:ys) | x == y = Just ys
                   | otherwise = (y:) <$> tryDelete x ys
tryDelete _ [] = Nothing

modifyRef :: IORef a -> (a -> (a, b)) -> IO b
modifyRef r f = do
  x <- readIORef r
  let (x', y) = f x
  writeIORef r x'
  return y
