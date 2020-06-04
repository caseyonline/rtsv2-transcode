module Rtsv2.Agents.IngestRtmpServer
       (
         startLink
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Function.Uncurried (Fn2, Fn5, mkFn2, mkFn5)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Logger as Logger
import Media.Rtmp as Rtmp
import Media.SourceDetails as SourceDetails
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen as Gen
import Rtsv2.Agents.IngestInstance (getPublishCredentials, getStreamAuthType, getStreamDetails)
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Agents.IngestRtmpCrypto (AdobeContextParams, AdobePhase1Params, AdobePhase2Params, LlnwContextParams, LlnwPhase1Params, LlnwPhase2Params, Phase2Params(..), checkCredentials)
import Rtsv2.Agents.IngestRtmpCrypto as IngestRtmpCrypto
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Types (LocalResource(..))
import Rtsv2.Utils (crashIfLeft, noprocToMaybe)
import Serf (Ip)
import Shared.Common (ProfileContext)
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.LlnwApiTypes (AuthType, SlotProfile(..), SlotPublishAuthType(..), StreamDetails, StreamIngestProtocol(..))
import Shared.Rtsv2.Stream (IngestKey(..))
import Shared.Rtsv2.Types (CanaryState(..), extractAddress)
import Stetson.WebSocketHandler (self)

foreign import startServerImpl :: (Foreign -> Either Foreign Unit) -> Either Foreign Unit -> Ip -> Int -> Callbacks -> Ip -> Int -> Callbacks -> Int -> Effect (Either Foreign Unit)
foreign import rtmpQueryToPurs :: Foreign -> RtmpAuthRequest
foreign import startWorkflowImpl :: Pid -> Pid -> Foreign -> IngestKey -> ProfileContext -> (Foreign -> (Effect Unit)) -> (Foreign -> (Effect Unit)) -> Effect Unit

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
  = { init :: Fn2 String Foreign (Effect RtmpAuthResponse)
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
  publicInterfaceIp <- Env.publicInterfaceIp
  supportInterfaceIp <- Env.supportInterfaceIp
  loadConfig <- Config.loadConfig
  {port, canaryPort, nbAcceptors} <- Config.rtmpIngestConfig
  thisServer <- PoPDefinition.getThisServer
  let
    host = unwrap $ extractAddress thisServer
    callbacks :: Callbacks
    callbacks = { init: mkFn2 (onConnectCallback loadConfig Live host)
                }
    canaryCallbacks :: Callbacks
    canaryCallbacks = { init: mkFn2 (onConnectCallback loadConfig Canary host)
                      }
  crashIfLeft =<< startServerImpl Left (Right unit) publicInterfaceIp port callbacks supportInterfaceIp canaryPort canaryCallbacks nbAcceptors
  pure $ {}

onConnectCallback :: LoadConfig -> CanaryState -> String -> String -> Foreign -> (Effect RtmpAuthResponse)
onConnectCallback loadConfig canary host rtmpShortName foreignQuery =
  let
    authRequest = rtmpQueryToPurs foreignQuery
  in
    processAuthRequest loadConfig canary host rtmpShortName authRequest

onStreamCallback :: LoadConfig -> CanaryState -> String -> String -> String -> String -> Int -> String -> Pid -> Foreign -> Effect Unit
onStreamCallback loadConfig canary host rtmpShortNameStr username remoteAddress remotePort rtmpStreamNameStr rtmpPid publishArgs = do
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
    Nothing -> do
      pure unit

    Just streamDetails -> do
      case findProfile rtmpStreamName streamDetails of
        Nothing ->
          pure unit

        Just (SlotProfile { name: profileName }) -> do
          let
            ingestKey = makeIngestKey profileName streamDetails
          self <- self
          maybeStarted <- IngestInstanceSup.startLocalRtmpIngest loadConfig canary ingestKey streamPublish streamDetails remoteAddress remotePort self
          case maybeStarted of
            Right (LocalResource ingestPid _server) -> do
              startWorkflowAndBlock rtmpPid ingestPid publishArgs ingestKey streamDetails
              _ <- noprocToMaybe $ IngestInstance.stopIngest ingestKey
              pure unit
            Left error -> do
              _ <- logWarning "Attempt to start local RTMP ingest failed" {error}
              pure unit
  where
    findProfile ingestStreamName streamDetails@{ slot: { profiles } } =
      find (\ (SlotProfile { rtmpStreamName: profileStreamName }) -> profileStreamName == ingestStreamName) profiles

    makeIngestKey profileName {role, slot: {id: slotId}} =
      IngestKey slotId role profileName

processAuthRequest :: LoadConfig -> CanaryState -> String -> String -> RtmpAuthRequest -> Effect RtmpAuthResponse
processAuthRequest _loadConfig _canary host rtmpShortName Initial = do
  authType <- getStreamAuthType host rtmpShortName
  pure $ fromMaybe RejectRequest $ InitialResponse <$> authType

processAuthRequest _loadConfig _canary host rtmpShortName (AdobePhase1 {username}) = do
  context <- IngestRtmpCrypto.newAdobeContext username
  pure $ AdobePhase1Response username context

processAuthRequest _loadConfig _canary host rtmpShortName (LlnwPhase1 {username}) = do
  context <- IngestRtmpCrypto.newLlnwContext username
  pure $ LlnwPhase1Response username context

processAuthRequest loadConfig canary host rtmpShortName (AdobePhase2 authParams@{username}) =
  processPhase2Authentication loadConfig canary host rtmpShortName username (AdobePhase2P authParams)

processAuthRequest loadConfig canary host rtmpShortName (LlnwPhase2 authParams@{username}) =
  processPhase2Authentication loadConfig canary host rtmpShortName username (LlnwPhase2P authParams)

processPhase2Authentication :: LoadConfig -> CanaryState -> String -> String -> String -> Phase2Params -> Effect RtmpAuthResponse
processPhase2Authentication loadConfig canary host rtmpShortName username authParams = do
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
      if ok then pure $ AcceptRequest (mkFn5 (onStreamCallback loadConfig canary host rtmpShortName username))
      else pure RejectRequest

startWorkflowAndBlock :: Pid -> Pid -> Foreign -> IngestKey -> StreamDetails -> Effect Unit
startWorkflowAndBlock rtmpPid ingestPid publishArgs ingestKey {slot: {name}} =
  startWorkflowImpl rtmpPid ingestPid publishArgs ingestKey profileMetadata clientMetadata sourceInfo
  where
    IngestKey slotId slotRole profileName = ingestKey

    profileMetadata = { slotId, slotRole, profileName, slotName: name}

    clientMetadata foreignMetadata = do
      IngestInstance.setClientMetadata ingestKey (Rtmp.foreignToMetadata foreignMetadata)

    sourceInfo foreignSourceInfo = do
      IngestInstance.setSourceInfo ingestKey (SourceDetails.foreignToSourceInfo foreignSourceInfo)

domain :: List Atom
domain = atom <$> (show Agent.Ingest : "Instance" : nil)

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo = Logger.info <<< Logger.traceMetadata domain

logWarning :: forall report. String -> { | report } -> Effect Unit
logWarning = Logger.warning <<< Logger.traceMetadata domain
