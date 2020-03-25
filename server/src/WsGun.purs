module WsGun
       (
         openWebSocket
       , messageMapper
       , processMessage
       , send
       , GunMsg(..)
       , ConnPid
       , GunState
       , Protocol
       , StreamRef
       , Reason
       , WsFrame
       , ProcessResponse(..)
       , GunProtocolResponse(..)
       , GunProtocolError(..)
       ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.Binary (Binary)
import Erl.Data.List (List)
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

data GunMsg = GunUp ConnPid Protocol
            | GunDown ConnPid Protocol Reason (List StreamRef) (List StreamRef)
            | GunUpgrade ConnPid StreamRef (List StreamRef)
            | GunWsFrame ConnPid StreamRef WsFrame
            | GunWsConnectionError ConnPid Reason
            | GunWsStreamError ConnPid StreamRef Reason
            | GunWsSendPing

data WsFrame = Close (Maybe Int) (Maybe String)
             | Ping (Maybe String)
             | Pong (Maybe String)
             | Text String
             | Binary Binary

data GunState clientMsg serverMsg = GunState { connPid :: Pid
                                             , path :: String
                                             }

data GunProtocolResponse = Up StreamRef
                         | Down
                         | Upgrade
                         | PingSent
                         | PingReceived
                         | PongReceived
                         | BinaryReceived
                         | CloseReceived

data GunProtocolError = StreamError
                      | ConnectionError
                      | ParseError

data ProcessResponse serverMsg = ProtocolResponse GunProtocolResponse
                               | Frame serverMsg

foreign import openImpl :: forall clientMsg serverMsg. Url -> Effect (Either Foreign (GunState clientMsg serverMsg))
foreign import upgradeImpl :: Pid -> String -> Effect StreamRef
foreign import messageMapperImpl :: Foreign -> Maybe GunMsg
foreign import pingImpl :: Pid -> Effect Unit
foreign import sendImpl :: Pid -> String -> Effect Unit

openWebSocket :: forall clientMsg serverMsg. Url -> Effect (Either Foreign (GunState clientMsg serverMsg))
openWebSocket url =
  openImpl url

processMessage :: forall clientMsg serverMsg. ReadForeign serverMsg => GunState clientMsg serverMsg -> GunMsg -> Effect (Either GunProtocolError (ProcessResponse serverMsg))
processMessage (GunState {connPid, path}) (GunUp _ _) =
  Right <$> ProtocolResponse <$> Up <$> upgradeImpl connPid path

processMessage _state (GunDown _ _ _ _ _) =
  pure $ Right $ ProtocolResponse $ Down

processMessage _state (GunUpgrade _ _ _) =
  pure $ Right $ ProtocolResponse $ Upgrade

processMessage _state (GunWsFrame _ _ (Close int str)) =
  pure $ Right $ ProtocolResponse $ CloseReceived

processMessage _state (GunWsFrame _ _ (Ping str)) =
  pure $ Right $ ProtocolResponse $ PingReceived

processMessage _state (GunWsFrame _ _ (Pong str)) =
  pure $ Right $ ProtocolResponse $ PongReceived

processMessage _state (GunWsFrame _ _ (Binary bin)) =
  pure $ Right $ ProtocolResponse $ BinaryReceived

processMessage _state (GunWsFrame _ _ (Text str)) =
  pure $ (lmap (const ParseError)) $ Frame <$> readJSON str

processMessage (GunState {connPid}) GunWsSendPing = do
  pingImpl connPid
  pure $ Right $ ProtocolResponse $ PingSent

processMessage _state (GunWsConnectionError _ _) =
  pure $ Left ConnectionError

processMessage _state (GunWsStreamError _ _ _) =
  pure $ Left StreamError

messageMapper :: Foreign -> Maybe GunMsg
messageMapper = messageMapperImpl

send :: forall clientMsg serverMsg. WriteForeign clientMsg => GunState clientMsg serverMsg -> clientMsg -> Effect Unit
send (GunState {connPid}) msg =
  sendImpl connPid (writeJSON msg)
