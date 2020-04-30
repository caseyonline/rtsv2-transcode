module Rtsv2.Agents.IngestRtmpServer
       (
         startLink
       )
       where

import Prelude

import Data.Either (Either(..), hush)
import Data.Foldable (find)
import Data.Function.Uncurried (Fn3, Fn5, mkFn3, mkFn5)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Logger (Logger)
import Logger as Logger
import Media.Rtmp as Rtmp
import Media.SourceDetails as SourceDetails
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen as Gen
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Agents.IngestRtmpCrypto (AdobeContextParams, AdobePhase1Params, AdobePhase2Params, LlnwContextParams, LlnwPhase1Params, LlnwPhase2Params, Phase2Params(..), checkCredentials)
import Rtsv2.Agents.IngestRtmpCrypto as IngestRtmpCrypto
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Names as Names
import Rtsv2.Utils (crashIfLeft)
import Serf (Ip)
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.LlnwApiTypes (AuthType, PublishCredentials, SlotProfile(..), SlotPublishAuthType(..), StreamAuth, StreamConnection, StreamDetails, StreamIngestProtocol(..), StreamPublish)
import Shared.Rtsv2.Stream (IngestKey(..))
import SpudGun (JsonResponseError, bodyToJSON)
import SpudGun as SpudGun
import Stetson.WebSocketHandler (self)

foreign import startServerImpl :: (Foreign -> Either Foreign Unit) -> Either Foreign Unit -> Ip -> Int -> Int -> Callbacks -> Effect (Either Foreign Unit)
foreign import rtmpQueryToPurs :: Foreign -> RtmpAuthRequest
foreign import startWorkflowImpl :: Pid -> Foreign -> IngestKey -> (Foreign -> (Effect Unit)) -> (Foreign -> (Effect Unit)) -> Effect Unit

data RtmpAuthRequest = Initial
                     | AdobePhase1 AdobePhase1Params
                     | AdobePhase2 AdobePhase2Params
                     | LlnwPhase1 LlnwPhase1Params
                     | LlnwPhase2 LlnwPhase2Params

data RtmpAuthResponse = InitialResponse AuthType
                      | AdobePhase1Response String AdobeContextParams
                      | LlnwPhase1Response String LlnwContextParams
                      | RejectRequest
                      | AcceptRequest (Fn5 String Int String Pid Foreign (Effect Unit))

type Callbacks
  = { init :: Fn3 String String Foreign (Effect RtmpAuthResponse)
    }

serverName :: ServerName State Unit
serverName = Names.ingestRtmpServerName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink args = Gen.startLink serverName (init args) Gen.defaultHandleInfo

type State =
  {
  }

init :: forall a. a -> Effect State
init _ = do
  interfaceIp <- Env.publicInterfaceIp
  loadConfig <- Config.loadConfig
  {port, nbAcceptors} <- Config.rtmpIngestConfig
  {streamAuthTypeUrl, streamAuthUrl, streamPublishUrl} <- Config.llnwApiConfig
  let
    callbacks :: Callbacks
    callbacks = { init: mkFn3 (onConnectCallback loadConfig)
                }
  crashIfLeft =<< startServerImpl Left (Right unit) interfaceIp port nbAcceptors callbacks
  pure $ {}

onConnectCallback :: LoadConfig -> String -> String -> Foreign -> (Effect RtmpAuthResponse)
onConnectCallback loadConfig host rtmpShortName foreignQuery =
  let
    authRequest = rtmpQueryToPurs foreignQuery
  in
    processAuthRequest loadConfig host rtmpShortName authRequest

onStreamCallback :: LoadConfig -> String -> String -> String -> String -> Int -> String -> Pid -> Foreign -> Effect Unit
onStreamCallback loadConfig host rtmpShortNameStr username remoteAddress remotePort rtmpStreamNameStr rtmpPid publishArgs = do
  let
    rtmpShortName = wrap rtmpShortNameStr
    rtmpStreamName = wrap rtmpStreamNameStr
    streamPublish = wrap { host
                         , protocol: Rtmp
                         , rtmpShortName
                         , rtmpStreamName
                         , username
                         }
  maybeStreamDetails <- getStreamDetails streamPublish
  case maybeStreamDetails of
    Left error -> do
      _ <- logInfo "StreamPublish rejected" {reason: error}
      pure unit

    Right streamDetails -> do
      case findProfile rtmpStreamName streamDetails of
        Nothing ->
          pure unit

        Just (SlotProfile { name: profileName }) -> do
          let
            ingestKey = makeIngestKey profileName streamDetails
          self <- self
          maybeStarted <- IngestInstanceSup.startLocalRtmpIngest loadConfig ingestKey streamPublish streamDetails remoteAddress remotePort self
          case maybeStarted of
            Right _ -> do
              startWorkflowAndBlock rtmpPid publishArgs ingestKey
              IngestInstance.stopIngest ingestKey
              pure unit
            Left error -> do
              _ <- logWarning "Attempt to start local RTMP ingest failed" {error}
              pure unit
  where
    findProfile ingestStreamName streamDetails@{ slot: { profiles } } =
      find (\ (SlotProfile { rtmpStreamName: profileStreamName }) -> profileStreamName == ingestStreamName) profiles

    makeIngestKey profileName {role, slot: {id: slotId}} =
      IngestKey slotId role profileName

processAuthRequest :: LoadConfig -> String -> String -> RtmpAuthRequest -> Effect RtmpAuthResponse
processAuthRequest _loadConfig host rtmpShortName Initial = do
  authType <- getStreamAuthType host rtmpShortName
  pure $ fromMaybe RejectRequest $ InitialResponse <$> authType

processAuthRequest _loadConfig host rtmpShortName (AdobePhase1 {username}) = do
  context <- IngestRtmpCrypto.newAdobeContext username
  pure $ AdobePhase1Response username context

processAuthRequest _loadConfig host rtmpShortName (LlnwPhase1 {username}) = do
  context <- IngestRtmpCrypto.newLlnwContext username
  pure $ LlnwPhase1Response username context

processAuthRequest loadConfig host rtmpShortName (AdobePhase2 authParams@{username}) =
  processPhase2Authentication loadConfig host rtmpShortName username (AdobePhase2P authParams)

processAuthRequest loadConfig host rtmpShortName (LlnwPhase2 authParams@{username}) =
  processPhase2Authentication loadConfig host rtmpShortName username (LlnwPhase2P authParams)

processPhase2Authentication :: LoadConfig -> String -> String -> String -> Phase2Params -> Effect RtmpAuthResponse
processPhase2Authentication loadConfig host rtmpShortName username authParams = do
  let
    authType = case authParams of
                 AdobePhase2P _ -> Adobe
                 LlnwPhase2P _ -> Llnw
  maybePublishCredentials <- getPublishCredentials host rtmpShortName username
  case maybePublishCredentials of
    Nothing ->
      pure RejectRequest
    Just publishCredentials -> do
      ok <- checkCredentials host rtmpShortName username publishCredentials authParams
      if ok then pure $ AcceptRequest (mkFn5 (onStreamCallback loadConfig host rtmpShortName username))
      else pure RejectRequest

startWorkflowAndBlock :: Pid -> Foreign -> IngestKey -> Effect Unit
startWorkflowAndBlock rtmpPid publishArgs ingestKey =
  startWorkflowImpl rtmpPid publishArgs ingestKey clientMetadata sourceInfo
  where
    clientMetadata foreignMetadata = do
      IngestInstance.setClientMetadata ingestKey (Rtmp.foreignToMetadata foreignMetadata)

    sourceInfo foreignSourceInfo = do
      IngestInstance.setSourceInfo ingestKey (SourceDetails.foreignToSourceInfo foreignSourceInfo)

getStreamAuthType :: String -> String -> Effect (Maybe AuthType)
getStreamAuthType host rtmpShortName = do
  {streamAuthTypeUrl: url} <- Config.llnwApiConfig
  restResult <- SpudGun.postJson (wrap url) (wrap { host
                                                  , protocol: Rtmp
                                                  , rtmpShortName: wrap rtmpShortName} :: StreamConnection)
  pure $ hush (bodyToJSON restResult)

getPublishCredentials :: String -> String -> String -> Effect (Maybe PublishCredentials)
getPublishCredentials host rtmpShortName username = do
  {streamAuthUrl: url} <- Config.llnwApiConfig
  restResult <- SpudGun.postJson (wrap url) (wrap { host
                                                  , rtmpShortName: wrap rtmpShortName
                                                  , username} :: StreamAuth)
  pure $ hush $ bodyToJSON restResult

getStreamDetails :: StreamPublish -> Effect (Either JsonResponseError StreamDetails)
getStreamDetails streamPublish = do
  {streamPublishUrl: url} <- Config.llnwApiConfig
  restResult <- SpudGun.postJson (wrap url) streamPublish
  pure $ bodyToJSON restResult

domain :: List Atom
domain = atom <$> (show Agent.Ingest : "Instance" : nil)

logInfo :: forall a. Logger (Record a)
logInfo = Logger.doLog domain Logger.info

logWarning :: forall a. Logger (Record a)
logWarning = Logger.doLog domain Logger.warning
