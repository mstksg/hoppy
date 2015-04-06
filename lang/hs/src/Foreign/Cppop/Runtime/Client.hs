-- | The Haskell Cppop runtime client manages communications with a Cppop
-- server.
--
-- A client uses a listener thread and multiple runner threads to handle
-- communications.  The listener simply waits for incoming messages.  When an
-- incoming call is received, then a new runner thread is created to execute
-- that call (except as described below).
--
-- When a piece of Haskell code issues a remote call, the issuing thread blocks
-- until the call returns.  If the remote thread handling the request makes a
-- nested call in exchange, then the first request id is used as the parent id
-- in the second, server-to-client request, so that the second request can be
-- routed to the existing thread.  In this way, for a series of dependent calls
-- that occur between a single Haskell thread and a single remote thread, the
-- Haskell and remote stacks can be viewed together as one large stack.
-- Simultaneous requests issued by other threads (from either end) are not
-- routed to existing listeners, because this could result in deadlock.
module Foreign.Cppop.Runtime.Client (
  Client,
  ClientParams (..),
  newClient,
  killClient,
  call,
  -- * Callbacks
  -- TODO Don't actually expose callbackId, make Callback serializable instead.
  Callback (callbackId),
  CallbackId,
  newCallback,
  deleteCallback,
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Exception (bracket, bracket_)
import Control.Monad (replicateM, void, when)
import Data.Binary.Get (Get, getWord8, runGetOrFail)
import Data.Binary.Put (putLazyByteString, putWord8, runPut)
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Int (Int32)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mappend)
import Data.Word (Word8)
import Foreign.C.Types (CSize)
import Foreign.Cppop.Runtime.Binary
import qualified Foreign.Cppop.Runtime.IdSpace as I
import Foreign.Storable (sizeOf)
import System.IO (IOMode (ReadMode, WriteMode), Handle, hFlush, hPutStrLn, openFile, stderr)

data ClientParams = ClientParams
  { paramInPath :: FilePath
  , paramOutPath :: FilePath
  }

data Client = Client
  { clientInHandle :: Handle
  , clientOutHandle :: Handle
  , clientOutMutex :: MVar ()
  , clientRequestIds :: I.ListIdSpace RequestId
  , clientRequests :: MVar (Map RequestId Request)
  , clientThreadRequestIds :: MVar (Map ThreadId [RequestId])
  , clientCallbackIds :: I.ListIdSpace CallbackId
    -- ^ TODO Don't use a list for these.
  , clientCallbacks :: MVar (Map CallbackId Callback)
  }

data Request = Request
  { requestClient :: Client
  , requestId :: RequestId
  , requestParentId :: Maybe RequestId
  , requestResponseVar :: MVar RecvMessage
    -- ^ Where to place the response when it arrives.
  }

newtype RequestId = RequestId Int32
                  deriving (Eq, HostBinary, Num, Ord, Show)

data RequestIdOwner = ServerOwner | ClientOwner

requestIdOwner :: RequestId -> RequestIdOwner
requestIdOwner (RequestId n) | n > 0 = ClientOwner
                             | n < 0 = ServerOwner
                             | otherwise = error "RequestId 0 is reserved."

data Callback = Callback
  { callbackClient :: Client
  , callbackId :: CallbackId
  , callbackAction :: CallbackAction
  }

newtype CallbackId = CallbackId Int32
                   deriving (Eq, HostBinary, Num, Ord, Show)

type CallbackAction = BL.ByteString -> IO BL.ByteString

calcWord8s :: [Word8]
calcWord8s = stringToWord8s "CALC"

callWord8s :: [Word8]
callWord8s = stringToWord8s "CALL"

retnWord8s :: [Word8]
retnWord8s = stringToWord8s "RETN"

stringToWord8s :: String -> [Word8]
stringToWord8s = map $ fromIntegral . ord

data RecvMessage =
  RecvCalc RequestId RequestId CallbackId BL.ByteString
  | RecvRetn RequestId BL.ByteString

instance Show RecvMessage where
  show (RecvCalc rid rpid cid body) =
    "(RecvCalc rid=" ++ show rid ++ " rpid=" ++ show rpid ++ " cid=" ++ show cid ++ ", " ++
    show (BL.length body) ++ "-byte body)"

  show (RecvRetn rid body) =
    "(RecvRetn rid=" ++ show rid ++ ", " ++ show (BL.length body) ++ "-byte body)"

data SendMessage =
  SendCall RequestId RequestId String BL.ByteString
  | SendRetn RequestId BL.ByteString

instance Show SendMessage where
  show (SendCall rid rpid name body) =
    "(SendCall rid=" ++ show rid ++ " rpid=" ++ show rpid ++ " name=" ++ show name ++ ", " ++
    show (BL.length body) ++ "-byte body)"

  show (SendRetn rid body) =
    "(SendRetn rid=" ++ show rid ++ ", " ++ show (BL.length body) ++ "-byte body)"

decodeRecvMessage :: BL.ByteString -> RecvMessage
decodeRecvMessage bytes =
  let result = flip runGetOrFail bytes $ do
        command <- replicateM 4 getWord8
        case () of
          _ | command == calcWord8s -> RecvCalc <$> hget <*> hget <*> hget
            | command == retnWord8s -> RecvRetn <$> hget
            | otherwise -> fail $ "Unknown command received: " ++ show command
  in case result of
    Left (_, _, errorMsg) -> error $ "decodeRecvMessage: " ++ errorMsg
    Right (body, _, f) -> f body

encodeSendMessage :: SendMessage -> BL.ByteString
encodeSendMessage msg =
  let bytes = runPut $ case msg of
        SendCall rid rpid fnName body -> do
          mapM_ putWord8 callWord8s
          hput rid
          hput rpid
          hput $ CString fnName
          putLazyByteString body
        SendRetn rid body -> do
          mapM_ putWord8 retnWord8s
          hput rid
          putLazyByteString body
      size :: CSize
      size = fromIntegral $ BL.length bytes  -- TODO Warning, casting.
  in runPut (hput size) `mappend` bytes

newClient :: ClientParams -> IO Client
newClient params = do
  outh <- openFile (paramOutPath params) WriteMode
  inh <- openFile (paramInPath params) ReadMode
  outMutex <- newMVar ()
  requestIds <- I.create "client request IDs" 1 2
  requests <- newMVar Map.empty
  threadRequestIds <- newMVar Map.empty
  callbackIds <- I.create "client callback IDs" 2 2
  callbacks <- newMVar Map.empty
  let client = Client
        { clientInHandle = inh
        , clientOutHandle = outh
        , clientOutMutex = outMutex
        , clientRequestIds = requestIds
        , clientRequests = requests
        , clientThreadRequestIds = threadRequestIds
        , clientCallbackIds = callbackIds
        , clientCallbacks = callbacks
        }
  --forkIO $ runRequestDispatcher client
  forkIO $ runResponseDistributor client
  return client

killClient :: Client -> IO ()
killClient = undefined  -- TODO

send :: Client -> BL.ByteString -> IO ()
send client bytes = withMutex (clientOutMutex client) $ do
  let outh = clientOutHandle client
  BL.hPut outh bytes
  hFlush outh

call :: Client -> String -> BL.ByteString -> IO BL.ByteString
call client functionName args =
  bracket (newEgressRequest client) releaseEgressRequest $ \request -> do
    send client $ encodeSendMessage $
      SendCall (requestId request)
               (fromMaybe 0 $ requestParentId request)
               functionName
               args
    let listen = do
          response <- takeMVar $ requestResponseVar request
          case response of
            RecvRetn responseId bodyBytes -> do
              when (responseId /= requestId request) $ do
                threadRequests <-
                  Map.lookup <$> myThreadId <*> readMVar (clientThreadRequestIds client)
                fail $ "Response ID " ++ show responseId ++ " doesn't match expected request ID " ++
                  show (requestId request) ++ ", thread has pending requests " ++
                  show threadRequests ++ "."
              return bodyBytes
            RecvCalc _ subParentId _ _ -> do
              when (subParentId /= requestId request) $
                fail $ "Expecting parent ID " ++ show (requestId request) ++
                " for received subcall " ++ show response ++ "."
              invokeHaskellCallback client response
              listen  -- Recur, we still need to wait on the outer frame.
    listen

invokeHaskellCallback :: Client -> RecvMessage -> IO ()
invokeHaskellCallback client message@(RecvCalc rid _ cid body) = case requestIdOwner rid of
  ClientOwner ->
    hPutStrLn stderr $
    "The server is trying to issue a callback subcall with a request ID it does not " ++
    "own, ignoring: " ++ show message ++ "."
  ServerOwner -> bracket_ (registerIngressRequestId client rid)
                          (releaseIngressRequestId client rid) $ do
    callback <- maybeFail ("Unknown callback ID in message: " ++ show message ++ ".") .
                Map.lookup cid =<<
                readMVar (clientCallbacks client)
    callbackResult <- callbackAction callback body
    send client $ encodeSendMessage $ SendRetn rid callbackResult
invokeHaskellCallback _ message =
  fail $ "invokeHaskellCallback only works for RecvCalc, not for: " ++ show message

runResponseDistributor :: Client -> IO ()
runResponseDistributor client = do
  let inh = clientInHandle client
      loop = do
        sizeBytes <- BL.hGet inh $ sizeOf (undefined :: CSize)
        when (BL.length sizeBytes > 0) $ do
          size <- case runGetOrFail (hget :: Get CSize) sizeBytes of
            Left (_, _, errorMsg) ->
              fail $ "runResponseDistributor reading message size: " ++ errorMsg
            Right (_, _, size) -> return size
          -- TODO Warning, casting.
          message <- decodeRecvMessage <$> BL.hGet inh (fromIntegral size)
          case message of

            RecvCalc _ rpid _ _
              | rpid == 0 -> void $ forkIO $ invokeHaskellCallback client message
              | otherwise ->
                  maybe (fail $ "Invalid parent request ID in received message, ignoring: " ++
                         show message)
                        (\parentRequest -> putMVar (requestResponseVar parentRequest) message) =<<
                  findRequest client rpid

            RecvRetn rid _ ->
              maybe (fail $ "Received a return for an unknown request ID, ignoring: " ++
                     show message)
                    (\request -> putMVar (requestResponseVar request) message) =<<
              findRequest client rid

        loop
  loop

registerIngressRequestId :: Client -> RequestId -> IO ()
registerIngressRequestId client rid = do
  threadId <- myThreadId
  modifyMVar_ (clientThreadRequestIds client) $
    return . Map.alter (Just . maybe [rid] (rid:)) threadId

releaseIngressRequestId :: Client -> RequestId -> IO ()
releaseIngressRequestId = releaseThreadRequestId

newEgressRequest :: Client -> IO Request
newEgressRequest client = do
  threadId <- myThreadId
  -- Obtain a request id.
  rid <- I.request $ clientRequestIds client
  -- Register the request id with the thread.
  rpid <- modifyMVar (clientThreadRequestIds client) $ return . \threadRequests ->
    case Map.lookup threadId threadRequests of
      Nothing -> (Map.insert threadId [rid] threadRequests, Nothing)
      Just (rpid:_) -> (Map.adjust (rid:) threadId threadRequests, Just rpid)
      Just [] -> error "newEgressRequest: Unexpected Just [] thread request ID list."
  -- Create a Request object and register it as well.
  responseVar <- newEmptyMVar
  let request = Request { requestClient = client
                        , requestId = rid
                        , requestParentId = rpid
                        , requestResponseVar = responseVar
                        }
  modifyMVar_ (clientRequests client) $ return . Map.insert rid request
  return request

releaseEgressRequest :: Request -> IO ()
releaseEgressRequest request = do
  let client = requestClient request
      rid = requestId request
  -- Unregister the request from the thread.
  releaseThreadRequestId client rid
  -- Release the Request object.
  request <- modifyMVar (clientRequests client) $ return . \requests ->
    case Map.lookup rid requests of
      Nothing -> (requests, Nothing)
      Just request -> (Map.delete rid requests, Just request)
  -- Release the request id.
  when (isJust request) $
    I.release (clientRequestIds client) rid

findRequest :: Client -> RequestId -> IO (Maybe Request)
findRequest client rid = Map.lookup rid <$> readMVar (clientRequests client)

releaseThreadRequestId :: Client -> RequestId -> IO ()
releaseThreadRequestId client rid = do
  threadId <- myThreadId
  modifyMVar_ (clientThreadRequestIds client) $ \threadRequests -> do
    -- Ensure that 'rid' is at the top of the stack.
    case Map.lookup threadId threadRequests of
      Just (ridTop:_) | ridTop == rid -> return ()
      rids -> fail $ "Trying to release request ID " ++ show rid ++
              ", but this thread has requests " ++ show rids ++ "."
    return $ Map.update ((\x -> if null x then Nothing else Just x) . tail)
                        threadId threadRequests

newCallback :: Client -> CallbackAction -> IO Callback
newCallback client action = do
  callbackId <- I.request $ clientCallbackIds client
  let callback = Callback { callbackClient = client
                          , callbackId = callbackId
                          , callbackAction = action
                          }
  modifyMVar_ (clientCallbacks client) $ return . Map.insert callbackId callback
  return callback

deleteCallback :: Callback -> IO ()
deleteCallback callback =
  modifyMVar_ (clientCallbacks $ callbackClient callback) $
  return . Map.delete (callbackId callback)

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

withMutex :: MVar () -> IO a -> IO a
withMutex var action = modifyMVar var $ \x -> (,) x <$> action

-- TODO Move this somewhere common and make other instances of "maybe
-- ... return" use this.
fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM = flip maybe return

maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail = fromMaybeM . fail
