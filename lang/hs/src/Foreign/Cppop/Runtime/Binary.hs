module Foreign.Cppop.Runtime.Binary where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM, when)
import Data.Binary.Get (Get, getByteString, getWord8, getWord16host, getWord32host, getWord64host)
import Data.Binary.Put (Put, putByteString, putWord8, putWord16host, putWord32host, putWord64host)
import Data.Bits ((.&.), (.|.), Bits, shift)
import qualified Data.ByteString as B
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (
  CInt,
  CIntPtr,
  CLLong,
  CLong,
  CShort,
  CSize,
  CUInt,
  CULLong,
  CULong,
  CUShort,
  )
import Foreign.Storable (Storable, sizeOf)

class HostBinary a where
  hget :: Get a
  hput :: a -> Put

instance HostBinary Word8 where
  hget = getWord8
  hput = putWord8

instance HostBinary Word16 where
  hget = getWord16host
  hput = putWord16host

instance HostBinary Word32 where
  hget = getWord32host
  hput = putWord32host

instance HostBinary Word64 where
  hget = getWord64host
  hput = putWord64host

instance HostBinary Int8 where
  hget = fmap fromIntegral (hget :: Get Word8)
  hput = hput . (fromIntegral :: Int8 -> Word8)

instance HostBinary Int16 where
  hget = fmap fromIntegral (hget :: Get Word16)
  hput = hput . (fromIntegral :: Int16 -> Word16)

instance HostBinary Int32 where
  hget = fmap fromIntegral (hget :: Get Word32)
  hput = hput . (fromIntegral :: Int32 -> Word32)

instance HostBinary Int64 where
  hget = fmap fromIntegral (hget :: Get Word64)
  hput = hput . (fromIntegral :: Int64 -> Word64)

-- TODO instance HostBinary Bool
instance HostBinary () where { hget = return (); hput _ = return () }
instance HostBinary CShort where { hget = getNum; hput = putNum }
instance HostBinary CUShort where { hget = getNum; hput = putNum }
instance HostBinary CInt where { hget = getNum; hput = putNum }
instance HostBinary CUInt where { hget = getNum; hput = putNum }
instance HostBinary CLong where { hget = getNum; hput = putNum }
instance HostBinary CULong where { hget = getNum; hput = putNum }
instance HostBinary CLLong where { hget = getNum; hput = putNum }
instance HostBinary CULLong where { hget = getNum; hput = putNum }
-- TODO CFloat, CDouble, don't have Bits instances here.
--instance HostBinary CFloat where { hget = getNum; hput = putNum }
--instance HostBinary CDouble where { hget = getNum; hput = putNum }
instance HostBinary CIntPtr where { hget = getNum; hput = putNum }
instance HostBinary CSize where { hget = getNum; hput = putNum }

{-
class (Bits a, Integral a, Storable a) => AutoHostBinary a

instance AutoHostBinary a => HostBinary a where
  hget = getNum
  hput = putNum

instance AutoHostBinary Word8

instance AutoHostBinary Int64

instance AutoHostBinary CSize
-}

getNum :: forall a. (Bits a, Integral a, Storable a) => Get a
getNum =
  -- TODO Don't assume little-endianness.
  foldr (\x y -> shift y 8 .|. fromIntegral x) 0 <$>
  replicateM (sizeOf (undefined :: a)) getWord8

putNum :: (Bits a, Integral a, Storable a) => a -> Put
putNum = putNum' <*> sizeOf

putNum' :: (Bits a, Integral a, Storable a) => a -> Int -> Put
putNum' x n =
  -- TODO Don't assume little-endianness.
  when (n > 0) $ do
    putWord8 $ fromIntegral $ x .&. 0xff
    putNum' (shift x (-8)) (n - 1)

instance HostBinary String where
  hget = do
    size <- hget :: Get CSize
    unpack . decodeUtf8 <$> getByteString (fromIntegral size)  -- TODO Warning, casting.

  hput str = do
    let bytes = encodeUtf8 $ pack str
    hput (fromIntegral $ B.length bytes :: CSize)  -- TODO Warning, casting.
    putByteString bytes

newtype CString = CString { fromCString :: String }

instance HostBinary CString where
  hget = undefined  -- TODO (Not sure if needed.)

  hput (CString str) = do
    putByteString $ encodeUtf8 $ pack str
    hput (0 :: Word8)

class HostBinary a => Ptr a where
  toPtr :: a -> CIntPtr
  fromPtr :: CIntPtr -> a

cast :: (Ptr a, Ptr b) => a -> b
cast = fromPtr . toPtr
