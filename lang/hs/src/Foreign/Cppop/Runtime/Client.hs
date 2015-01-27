module Foreign.Cppop.Runtime.Client (
  Client,
  ClientParams (..),
  newClient,
  killClient,
  send,
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (when)
import Data.Binary.Get (ByteOffset, runGetOrFail)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Monoid (mappend)
import Foreign.C.Types (CInt, CSize)
import Foreign.Cppop.Runtime.Binary
import qualified Foreign.Cppop.Runtime.IdSpace as I
import Foreign.Storable (Storable, sizeOf)
import System.IO (IOMode (ReadMode, WriteMode), Handle, hClose, hFlush, openFile)

newtype ClientRequestId = ClientRequestId CInt
                        deriving (Eq, HostBinary, Num, Ord, Show, Storable)

data ClientParams = ClientParams
  { paramInPath :: FilePath
  , paramOutPath :: FilePath
  }

data Client = Client
  { clientInHandle :: Handle
  , clientOutHandle :: Handle
  , clientRequestChannel :: Chan Request
  , clientRequests :: MVar (Map ClientRequestId Request)
  , clientRequestIds :: I.ListIdSpace ClientRequestId
  }

data Request = Request
  { requestBody :: Maybe BL.ByteString
    -- ^ The request to send to the server.  Includes function name and
    -- arguments.  Does not include sequence number or size word.
  , requestResponseVar :: MVar BL.ByteString
    -- ^ The var into which to stick the response when it arrives.
  }

data ServerResponseHeader = ServerResponseHeader
  { serverResponseId :: ClientRequestId
  , serverResponseBodySize :: CSize
  }

parseServerResponseHeader :: BL.ByteString
                          -> Either (BL.ByteString, ByteOffset, String)
                                    (BL.ByteString, ByteOffset, ServerResponseHeader)
parseServerResponseHeader input =
  runGetOrFail (ServerResponseHeader <$> hget <*> hget) input

newClient :: ClientParams -> IO Client
newClient params = do
  outh <- openFile (paramOutPath params) WriteMode
  inh <- openFile (paramInPath params) ReadMode
  requestChannel <- newChan
  requests <- newMVar Map.empty
  requestIds <- I.create "client request IDs" 1 2
  let client = Client
        { clientInHandle = inh
        , clientOutHandle = outh
        , clientRequestChannel = requestChannel
        , clientRequests = requests
        , clientRequestIds = requestIds
        }
  --forkIO $ runRequestDispatcher client
  forkIO $ runResponseDistributor client
  return client

killClient :: Client -> IO ()
killClient = undefined  -- TODO

send :: Client -> BL.ByteString -> IO BL.ByteString
send client bodyBytes = do
  -- TODO Thread safety.  Should synchronize with other writers.
  responseVar <- newEmptyMVar
  let request = Request { requestBody = Nothing
                        , requestResponseVar = responseVar
                        }
      -- TODO Potential casting problems!
      requestSize = fromIntegral $ BL.length bodyBytes :: CSize
  requestId <- pushRequest client request
  let outh = clientOutHandle client
      headerBytes = runPut $ do
        hput requestId
        hput requestSize
  BB.hPutBuilder outh $
    BB.lazyByteString headerBytes `mappend` BB.lazyByteString bodyBytes
  hFlush outh
  takeMVar responseVar

--sendAsync :: Client -> B.ByteString -> (B.ByteString -> IO ()) -> IO ()

--sendAsync_ :: Client -> B.ByteString -> IO ()

--runRequestDispatcher :: Client -> IO ()
--runRequestDispatcher client = do
--  let inh = clientInHandle client
--      outh = clientOutHandle client
--      chan = clientRequestChannel client
--  req <- readChan chan

runResponseDistributor :: Client -> IO ()
runResponseDistributor client = do
  let inh = clientInHandle client
      headerSize = sizeOf (undefined :: ClientRequestId) +
                   sizeOf (undefined :: CSize)
      loop = do
        headerBytes <- BL.hGet inh headerSize
        let result = flip runGetOrFail headerBytes $ do
              requestId <- hget
              bodySize <- hget
              return $ ServerResponseHeader { serverResponseId = requestId
                                            , serverResponseBodySize = bodySize
                                            }
        case result of
          Left (_, _, errorMsg) ->
            putStrLn $ "Received malformed header response (" ++ errorMsg ++ "), ignoring."
          Right (headerRemainder, _, header) ->
            if not $ BL.null headerRemainder
            then putStrLn $ "Internal parsing error, response header contains " ++
                 show (BL.length headerBytes) ++ " bytes, only consumed " ++
                 show (BL.length headerBytes - BL.length headerRemainder) ++ "."
            else do
              let requestId = serverResponseId header
                  bodySize = serverResponseBodySize header
              bodyBytes <- BL.hGet inh $ fromIntegral bodySize  -- TODO Potential casting problems!
              maybeRequest <- popRequest client requestId
              case maybeRequest of
                Nothing ->
                  putStrLn $ "Received response for sequence number " ++ show requestId ++
                  " with no active request."
                Just request ->
                  putMVar (requestResponseVar request) bodyBytes
        loop
  loop

-- | Adds a new 'Request' to the 'Requests' of a 'Client'.  Allocates and
-- returns a 'ClientRequestId' for the request.
pushRequest :: Client -> Request -> IO ClientRequestId
pushRequest client request = do
  requestId <- I.request $ clientRequestIds client
  modifyMVar_ (clientRequests client) $ return . Map.insert requestId request
  return requestId

-- | Retrieves a 'Request' from the 'Requests' of a 'Client' by its
-- 'ClientRequestId', and removes the request from the requests.
popRequest :: Client -> ClientRequestId -> IO (Maybe Request)
popRequest client requestId = do
  request <- modifyMVar (clientRequests client) $ return . \requests ->
    case Map.lookup requestId requests of
      Nothing -> (requests, Nothing)
      Just request -> (Map.delete requestId requests, Just request)
  when (isJust request) $
    I.release (clientRequestIds client) requestId
  return request

{-
runBracketed :: Client -> IO ()
runBracketed client =
  flip finally (hClose $ clientInHandle client) $
  flip finally (hClose $ clientOutHandle client) $
  run client

run :: Client -> IO ()
run client = do
  let inh = clientInHandle client
      outh = clientOutHandle client
      chan = clientRequestChannel client
  req <- readChan chan
  case req of
    RequestShutdown -> return ()
    RequestCall {} -> ...
-}
