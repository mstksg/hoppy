module Main where

import Control.Monad ((>=>), forM_)
import Foreign.Hoppy.Example.Instances
import Foreign.Hoppy.Example.Std
import Foreign.Hoppy.Runtime.Support (delete, encodeAs, withCppObj)

main :: IO ()
main = do
  putStrLn "Creating a std::vector<std::string> and putting some things in it."
  v <- vectorString_new
  vectorString_pushBack v "Hello"
  vectorString_pushBack v "there"
  s <- encodeAs (undefined :: StdString) "joyful"
  vectorString_pushBack v s
  withCppObj "world!" $ \s' -> vectorString_pushBack v (s' :: StdStringConst)

  -- Walk through the vector using random access.
  size <- vectorString_size v
  putStrLn $ concat ["The vector now has ", show size, " elements.  They are:"]
  forM_ [0..size-1] $
    vectorString_at v >=> print
  delete v
