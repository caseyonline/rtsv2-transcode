module WsGun
       (
         newContext
       , openWebSocket
       , messageMapper
       , processMessage
       , send
       , Context
       , GunMsg
       , ConnPid
       , GunState
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

data GunMsg = GunUp ConnPid Protocol
            | GunDown ConnPid Protocol Reason (List StreamRef) (List StreamRef)
            | GunUpgrade ConnPid StreamRef (List String)
            | GunWsFrame ConnPid StreamRef WsFrame
            | GunWsConnectionError ConnPid Reason
            | GunWsStreamError ConnPid StreamRef Reason
            | GunWsSendPing ConnPid

data WsFrame = Close (Maybe Int) (Maybe String)
             | Ping (Maybe String)
             | Pong (Maybe String)
             | Text String
             | Binary Binary

data Context clientMsg serverMsg okMsg errorMsg key = Context { connPidMap :: Map Pid (GunState clientMsg serverMsg okMsg errorMsg)
                                                              , keyMap :: Map key Pid
                                                              }

type WsGunInstance = { connPid :: Pid
                     , path :: String
                     , tref :: Ref
                     }

data GunState clientMsg serverMsg okMsg errorMsg = GunState WsGunInstance (ProcessResponse serverMsg -> okMsg) (GunProtocolError -> errorMsg)

data GunProtocolError = StreamError
                      | ConnectionError
                      | ParseError

data ProcessResponse serverMsg = Internal GunMsg
                               | WebSocketUp
                               | WebSocketDown
                               | Frame serverMsg

data ProcessError errorMsg = Error errorMsg
                           | UnknownSocket

foreign import openImpl :: Url -> Effect (Either Foreign WsGunInstance)
foreign import upgradeImpl :: Pid -> String -> Effect StreamRef
foreign import messageMapperImpl :: Foreign -> Maybe GunMsg
foreign import pingImpl :: Pid -> Effect Unit
foreign import sendImpl :: String -> Pid -> Effect Unit

newContext :: forall clientMsg serverMsg okMsg errorMsg key. Context clientMsg serverMsg okMsg errorMsg key
newContext = Context { connPidMap: Map.empty
                     , keyMap: Map.empty
                     }

openWebSocket :: forall clientMsg serverMsg okMsg errorMsg key. Ord key => Context clientMsg serverMsg okMsg errorMsg key -> (ProcessResponse serverMsg -> okMsg) -> (GunProtocolError -> errorMsg) -> key -> Url -> Effect (Either Foreign (Context clientMsg serverMsg okMsg errorMsg key))
openWebSocket (Context {connPidMap, keyMap}) okMsg errorMsg key url = do
  res <- openImpl url
  pure $ (\gun@{connPid} ->
           let
             newConnPidMap = Map.insert connPid (GunState gun okMsg errorMsg) connPidMap
             newKeyMap = Map.insert key connPid keyMap
           in
            Context { connPidMap: newConnPidMap
                    , keyMap: newKeyMap
                    }
         ) <$> res

processMessage :: forall clientMsg serverMsg okMsg errorMsg key. ReadForeign serverMsg => Context clientMsg serverMsg okMsg errorMsg key -> GunMsg -> Effect (Either (ProcessError errorMsg) okMsg)
processMessage context gunMsg@(GunUp connPid _protocol) = do
  let
    resp = getOkFun context gunMsg $ (Internal gunMsg)
  _ <- maybeUpgrade
  pure $ resp
  where
    maybeUpgrade =
      fromMaybe (pure unit) $ (\(GunState {path} _ _) -> (const unit) <$> upgradeImpl connPid path) <$> getGunState context gunMsg

processMessage context gunMsg@(GunDown _connPid _protocol _reason _killedStreams _unprocessedStreams) =
  pure $ getOkFun context gunMsg $ WebSocketDown

processMessage context gunMsg@(GunUpgrade _connPid _streamRef _headers) =
  pure $ getOkFun context gunMsg $ WebSocketUp

processMessage context gunMsg@(GunWsSendPing connPid) = do
  let
    resp = getOkFun context gunMsg $ (Internal gunMsg)
  _ <- maybePing
  pure $ resp
  where
    maybePing =
      fromMaybe (pure unit) $ (\(GunState {path} _ _) -> pingImpl connPid) <$> getGunState context gunMsg

processMessage context gunMsg@(GunWsConnectionError _connPid _reason) = do
  pure $ Left $ getErrorFun context gunMsg $ ConnectionError

processMessage context gunMsg@(GunWsStreamError _connPid _streamRef _reason) = do
  pure $ Left $ getErrorFun context gunMsg $ StreamError

processMessage context gunMsg@(GunWsFrame _connPid _streamRef (Close _int _str)) = do
  pure $ getOkFun context gunMsg $ Internal gunMsg

processMessage context gunMsg@(GunWsFrame connPid _streamRef (Ping str)) = do
  pure $ getOkFun context gunMsg $ Internal gunMsg

processMessage context gunMsg@(GunWsFrame connPid _streamRef (Pong str)) = do
  pure $ getOkFun context gunMsg $ Internal gunMsg

processMessage context gunMsg@(GunWsFrame connPid _streamRef (Binary bin)) = do
  pure $ getOkFun context gunMsg $ Internal gunMsg

processMessage context@(Context map) gunMsg@(GunWsFrame connPid _streamRef (Text str)) =
  let
    okFun = getOkFun context gunMsg
    errorFun = getErrorFun context gunMsg
  in
   pure $ case readJSON str of
     Left _ -> Left $ errorFun ParseError
     Right frame -> okFun (Frame frame)

send :: forall clientMsg serverMsg okMsg errorMsg key. Ord key => WriteForeign clientMsg => Context clientMsg serverMsg okMsg errorMsg key -> key -> clientMsg -> Effect (Either Unit Unit)
send context@(Context {connPidMap, keyMap}) key msg = do
  let
    mConnPid = Map.lookup key keyMap
  resp <- sequence $ sendImpl (writeJSON msg) <$> mConnPid
  pure $ note unit resp

messageMapper :: Foreign -> Maybe GunMsg
messageMapper = messageMapperImpl

------------------------------------------------------------------------------
-- Internal functions
------------------------------------------------------------------------------
getConnPid :: GunMsg -> ConnPid
getConnPid (GunUp connPid _protocol) = connPid
getConnPid (GunDown connPid _protocol _reason _killedStreams _unprocessedStreams) = connPid
getConnPid (GunUpgrade connPid _streamRef _headers) = connPid
getConnPid (GunWsSendPing connPid) = connPid
getConnPid (GunWsConnectionError connPid _reason) = connPid
getConnPid (GunWsStreamError connPid _streamRef _reason) = connPid
getConnPid (GunWsFrame connPid _streamRef _frame) = connPid

getGunState :: forall clientMsg serverMsg okMsg errorMsg key. Context clientMsg serverMsg okMsg errorMsg key -> GunMsg -> Maybe (GunState clientMsg serverMsg okMsg errorMsg)
getGunState context@(Context {connPidMap: map}) gunMsg =
  Map.lookup (getConnPid gunMsg) map

getOkFun :: forall clientMsg serverMsg okMsg errorMsg key. Context clientMsg serverMsg okMsg errorMsg key -> GunMsg -> (ProcessResponse serverMsg -> Either (ProcessError errorMsg) okMsg)
getOkFun context gunMsg =
  let
    mGunState = getGunState context gunMsg
    mFun = (\(GunState wsGunInstance okFun errorFun) -> okFun) <$> mGunState
  in
   fromMaybe (const $ Left UnknownSocket) ((\fn -> (\s -> Right $ fn s)) <$> mFun)

getErrorFun :: forall clientMsg serverMsg okMsg errorMsg key. Context clientMsg serverMsg okMsg errorMsg key -> GunMsg -> (GunProtocolError -> (ProcessError errorMsg))
getErrorFun context gunMsg =
  let
    mGunState = getGunState context gunMsg
    mFun = (\(GunState wsGunInstance okFun errorFun) -> errorFun) <$> mGunState
  in
   fromMaybe (const UnknownSocket) ((\fn -> (\e -> Error $ fn e)) <$> mFun)
