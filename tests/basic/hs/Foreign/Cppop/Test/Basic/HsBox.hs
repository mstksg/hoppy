module Foreign.Cppop.Test.Basic.HsBox where

import Foreign.C (CInt)

data HsBox = HsBox { getHsBox :: CInt }
           deriving (Eq, Show)
