module Main where

import Control.Monad (forM_)
import Foreign.Cppop.Runtime.Support (delete, encodeAs, withCppObj)
import Foreign.Cppop.Example.Instances
import Foreign.Cppop.Example.Std

main :: IO ()
main = do
  putStrLn "Creating a std::vector<std::string> and putting some things in it."
  v <- vectorString_new
  vectorString_pushBack v "Hello"
  vectorString_pushBack v "there"
  s <- encodeAs (undefined :: StdString) "joyful"
  vectorString_pushBack v s
  withCppObj "world!" $ \s' -> vectorString_pushBack v (s' :: StdStringConst)

  size <- vectorString_size v
  putStrLn $ concat ["The vector now has ", show size, " elements.  They are:"]
  forM_ [0..size-1] $ \i -> do
    vectorString_at v i >>= print
  delete v
