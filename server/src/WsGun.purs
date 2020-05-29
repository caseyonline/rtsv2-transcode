module WsGun
       ( openWebSocket
       , closeWebSocket
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
import Data.Int (round)
import Data.Long as Long
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2, fst, snd)
import Erl.Process.Raw (Pid)
import Erl.Utils (Ref)
import Erl.Utils as Erl
import Foreign (Foreign)
import Logger (spy)
import Pinto (ServerName)
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Shared.Common (Milliseconds(..), Url)
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
            | GunExit ConnPid
            | KeepAliveCheck ConnPid

data WebSocket clientMsg serverMsg = Connection ConnPid String Milliseconds

data GunProtocolError = StreamError
                      | ConnectionError
                      | ParseError

data ProcessResponse clientMsg serverMsg = Internal GunMsg
                                         | WebSocketUp
                                         | WebSocketDown
                                         | WebSocketUpdate (WebSocket clientMsg serverMsg)
                                         | Frame serverMsg

data ProcessError errorMsg = Error errorMsg
                           | UnknownSocket

foreign import openImpl :: Url -> Int -> Effect (Either Foreign (Tuple2 ConnPid String))
foreign import closeImpl :: ConnPid -> Effect Unit
foreign import upgradeImpl :: Pid -> String -> Effect StreamRef
foreign import messageMapperImpl :: Foreign -> Maybe GunMsg
foreign import sendImpl :: String -> Pid -> Effect Unit

keepalive :: Int
keepalive = 5000

numMissingKeepalives :: Int
numMissingKeepalives = 3

openWebSocket :: forall state msg clientMsg serverMsg. ServerName state msg -> (GunMsg -> msg) -> Url -> Effect (Either Foreign (WebSocket clientMsg serverMsg))
openWebSocket serverName mapper url = do
  res <- openImpl url keepalive
  case res of
    Left err ->
      pure $ Left err
    Right tuple -> do
      let
        pid = fst tuple
        path = snd tuple
      now <- Erl.systemTimeMs
      Gen.monitorPid serverName pid (\_ -> mapper $ GunExit pid)
      _ <- Timer.sendEvery serverName keepalive (mapper (KeepAliveCheck pid))
      pure $ Right $ Connection (fst tuple) (snd tuple) now

closeWebSocket :: forall clientMsg serverMsg. WebSocket clientMsg serverMsg -> Effect Unit
closeWebSocket (Connection connPid _path _) =
  closeImpl connPid

processMessage :: forall clientMsg serverMsg. ReadForeign serverMsg => WebSocket clientMsg serverMsg -> GunMsg -> Effect (Either GunProtocolError (ProcessResponse clientMsg serverMsg))
processMessage _socket@(Connection _connPid path _)  gunMsg@(GunUp connPid _protocol) = do
  _ <- upgradeImpl connPid path
  pure $ Right $ Internal gunMsg

processMessage _socket gunMsg@(GunDown _connPid _protocol _reason _killedStreams _unprocessedStreams) =
  pure $ Right WebSocketDown

processMessage _socket gunMsg@(GunExit _connPid) =
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

processMessage _socket@(Connection pid path _lastKeepalive) gunMsg@(GunWsFrame _connPid _streamRef (Pong str)) = do
  now <- Erl.systemTimeMs
  pure $ Right $ WebSocketUpdate $ Connection pid path now

processMessage _socket gunMsg@(GunWsFrame _connPid _streamRef (Binary bin)) = do
  pure $ Right $ Internal gunMsg

processMessage _socket gunMsg@(GunWsFrame _connPid _streamRef (Text str)) =
  pure $ case readJSON str of
    Left _ -> Left ParseError
    Right frame -> Right $ Frame frame

processMessage socket@(Connection connPid path lastKeepalive) gunMsg@(KeepAliveCheck _connPid) = do
  now <- Erl.systemTimeMs
  if unwrap now - unwrap lastKeepalive > Long.fromInt (keepalive * numMissingKeepalives) then do
    closeImpl connPid
    pure $ Right WebSocketDown
  else
    pure $ Right $ Internal gunMsg

send :: forall clientMsg serverMsg. WriteForeign clientMsg => WebSocket clientMsg serverMsg -> clientMsg -> Effect Unit
send (Connection connPid _ _) msg =
  sendImpl (writeJSON msg) connPid

isSocketForMessage :: forall clientMsg serverMsg. GunMsg -> WebSocket clientMsg serverMsg -> Boolean
isSocketForMessage msg (Connection connPid _ _) =
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
getConnPid (GunExit connPid) = connPid
getConnPid (KeepAliveCheck connPid) = connPid
