module WsGun
       ( openWebSocket
       , messageMapper
       , processMessage
       , send
       , isSocketForMessage
       , GunMsg
       , WebSocket
       , ConnPid
       , Protocol
       , StreamRef
       , Reason
       , ProcessResponse(..)
       , ProcessError(..)
       , GunProtocolError(..)
       ) where

import Prelude

import Data.Either (Either(..), note)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2)
import Erl.Process.Raw (Pid)
import Erl.Utils (Ref)
import Foreign (Foreign)
import Logger (spy)
import Shared.Common (Url)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

data Protocol = HTTP
              | HTTP2
              | Socks
              | WebSocket

type Headers = List (Tuple2 String String)
type StreamRef = Ref
type ConnPid = Pid
type Reason = Foreign

data WsFrame = Close (Maybe Int) (Maybe String)
             | Ping (Maybe String)
             | Pong (Maybe String)
             | Text String
             | Binary Binary

data GunMsg = GunUp ConnPid Protocol
            | GunDown ConnPid Protocol Reason (List StreamRef) (List StreamRef)
            | GunUpgrade ConnPid StreamRef (List String)
            | GunWsFrame ConnPid StreamRef WsFrame
            | GunWsConnectionError ConnPid Reason
            | GunWsStreamError ConnPid StreamRef Reason

data WebSocket clientMsg serverMsg = Connection ConnPid

data GunProtocolError = StreamError
                      | ConnectionError
                      | ParseError

data ProcessResponse serverMsg = Internal GunMsg
                               | WebSocketUp
                               | WebSocketDown
                               | Frame serverMsg

data ProcessError errorMsg = Error errorMsg
                           | UnknownSocket

foreign import openImpl :: Url -> Effect (Either Foreign ConnPid)
foreign import upgradeImpl :: Pid -> Effect StreamRef
foreign import messageMapperImpl :: Foreign -> Maybe GunMsg
foreign import sendImpl :: String -> Pid -> Effect Unit

openWebSocket :: forall clientMsg serverMsg. Url -> Effect (Either Foreign (WebSocket clientMsg serverMsg))
openWebSocket url = do
  res <- openImpl url
  pure $ Connection <$> res

processMessage :: forall clientMsg serverMsg. ReadForeign serverMsg => WebSocket clientMsg serverMsg -> GunMsg -> Effect (Either GunProtocolError (ProcessResponse serverMsg))
processMessage _socket gunMsg@(GunUp connPid _protocol) = do
  _ <- upgradeImpl connPid
  pure $ Right $ Internal gunMsg

processMessage _socket gunMsg@(GunDown _connPid _protocol _reason _killedStreams _unprocessedStreams) =
  pure $ Right WebSocketDown

processMessage _socket gunMsg@(GunUpgrade _connPid _streamRef _headers) =
  pure $ Right WebSocketUp

processMessage _socket gunMsg@(GunWsConnectionError _connPid _reason) = do
  pure $ Left ConnectionError

processMessage _socket gunMsg@(GunWsStreamError _connPid _streamRef _reason) = do
  pure $ Left StreamError

processMessage _socket gunMsg@(GunWsFrame _connPid _streamRef (Close _int _str)) = do
  pure $ Right $ Internal gunMsg

processMessage _socket gunMsg@(GunWsFrame _connPid _streamRef (Ping str)) = do
  pure $ Right $ Internal gunMsg

processMessage _socket gunMsg@(GunWsFrame _connPid _streamRef (Pong str)) = do
  pure $ Right $ Internal gunMsg

processMessage _socket gunMsg@(GunWsFrame _connPid _streamRef (Binary bin)) = do
  pure $ Right $ Internal gunMsg

processMessage _socket gunMsg@(GunWsFrame _connPid _streamRef (Text str)) =
  pure $ case readJSON str of
    Left _ -> Left ParseError
    Right frame -> Right $ Frame frame

send :: forall clientMsg serverMsg. WriteForeign clientMsg => WebSocket clientMsg serverMsg -> clientMsg -> Effect Unit
send (Connection connPid) msg =
  sendImpl (writeJSON msg) connPid

isSocketForMessage :: forall clientMsg serverMsg. GunMsg -> WebSocket clientMsg serverMsg -> Boolean
isSocketForMessage msg (Connection connPid) =
  getConnPid msg == connPid

messageMapper :: Foreign -> Maybe GunMsg
messageMapper = messageMapperImpl

-- ------------------------------------------------------------------------------
-- -- Internal functions
-- ------------------------------------------------------------------------------
getConnPid :: GunMsg -> ConnPid
getConnPid (GunUp connPid _protocol) = connPid
getConnPid (GunDown connPid _protocol _reason _killedStreams _unprocessedStreams) = connPid
getConnPid (GunUpgrade connPid _streamRef _headers) = connPid
getConnPid (GunWsConnectionError connPid _reason) = connPid
getConnPid (GunWsStreamError connPid _streamRef _reason) = connPid
getConnPid (GunWsFrame connPid _streamRef _frame) = connPid

-- getGunState :: forall clientMsg serverMsg okMsg errorMsg key. Context clientMsg serverMsg okMsg errorMsg key -> GunMsg -> Maybe (GunState clientMsg serverMsg okMsg errorMsg)
-- getGunState context@(Context {connPidMap: map}) gunMsg =
--   Map.lookup (getConnPid gunMsg) map

-- getOkFun :: forall clientMsg serverMsg okMsg errorMsg key. Context clientMsg serverMsg okMsg errorMsg key -> GunMsg -> (ProcessResponse serverMsg -> Either (ProcessError errorMsg) okMsg)
-- getOkFun context gunMsg =
--   let
--     mGunState = getGunState context gunMsg
--     mFun = (\(GunState wsGunInstance okFun errorFun) -> okFun) <$> mGunState
--   in
--    fromMaybe (const $ Left UnknownSocket) ((\fn -> (\s -> Right $ fn s)) <$> mFun)

-- getErrorFun :: forall clientMsg serverMsg okMsg errorMsg key. Context clientMsg serverMsg okMsg errorMsg key -> GunMsg -> (GunProtocolError -> (ProcessError errorMsg))
-- getErrorFun context gunMsg =
--   let
--     mGunState = getGunState context gunMsg
--     mFun = (\(GunState wsGunInstance okFun errorFun) -> errorFun) <$> mGunState
--   in
--    fromMaybe (const UnknownSocket) ((\fn -> (\e -> Error $ fn e)) <$> mFun)
