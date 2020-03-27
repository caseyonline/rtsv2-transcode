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

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2, fst, snd)
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

data WebSocket clientMsg serverMsg = Connection ConnPid String

data GunProtocolError = StreamError
                      | ConnectionError
                      | ParseError

data ProcessResponse serverMsg = Internal GunMsg
                               | WebSocketUp
                               | WebSocketDown
                               | Frame serverMsg

data ProcessError errorMsg = Error errorMsg
                           | UnknownSocket

foreign import openImpl :: Url -> Effect (Either Foreign (Tuple2 ConnPid String))
foreign import upgradeImpl :: Pid -> String -> Effect StreamRef
foreign import messageMapperImpl :: Foreign -> Maybe GunMsg
foreign import sendImpl :: String -> Pid -> Effect Unit

openWebSocket :: forall clientMsg serverMsg. Url -> Effect (Either Foreign (WebSocket clientMsg serverMsg))
openWebSocket url = do
  res <- openImpl url
  pure $ (\tuple -> Connection (fst tuple) (snd tuple)) <$> res

processMessage :: forall clientMsg serverMsg. ReadForeign serverMsg => WebSocket clientMsg serverMsg -> GunMsg -> Effect (Either GunProtocolError (ProcessResponse serverMsg))
processMessage _socket@(Connection _connPid path)  gunMsg@(GunUp connPid _protocol) = do
  _ <- upgradeImpl connPid path
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
send (Connection connPid _) msg =
  sendImpl (writeJSON msg) connPid

isSocketForMessage :: forall clientMsg serverMsg. GunMsg -> WebSocket clientMsg serverMsg -> Boolean
isSocketForMessage msg (Connection connPid _) =
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
