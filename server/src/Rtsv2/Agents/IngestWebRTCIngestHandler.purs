module Rtsv2.Agents.IngestWebRTCIngestHandler
       (
         authenticate
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Process.Raw (Pid)
import Erl.Utils as Erl
import Foreign (Foreign)
import Logger as Logger
import Media.SourceDetails as SourceDetails
import Rtsv2.Agents.IngestInstance (getPublishCredentials, getStreamDetails)
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Config (LoadConfig)
import Rtsv2.DataObject as DO
import Rtsv2.Types (LocalResourceResp)
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.LlnwApiTypes (PublishCredentials(..), SlotProfile(..), StreamDetails, StreamIngestProtocol(..))
import Shared.Rtsv2.Stream (IngestKey(..), ProfileName, RtmpStreamName(..))
import Shared.Rtsv2.Types (CanaryState, Server)

foreign import startWorkflowImpl :: IngestKey -> Effect Pid

type AuthenticateResult = { streamDetails :: StreamDetails
                          , profileName :: ProfileName
                          , startStream :: Maybe (Effect (Maybe StartStreamResult))
                          , dataObjectSendMessage :: DO.Message -> Effect Unit
                          , dataObjectUpdate :: DO.ObjectUpdateMessage -> Effect Unit
                          }

type StartStreamResult = { sourceInfo :: Foreign -> Effect Unit
                         , workflowPid :: Pid
                         , stopStream :: Effect Unit
                         }

authenticate :: LoadConfig -> CanaryState -> String -> StreamIngestProtocol -> String -> String -> String -> String -> String -> Int -> Effect (Maybe AuthenticateResult)
authenticate loadConfig canary host protocol account username password streamName remoteAddress remotePort = do
  publishCredentials <- getPublishCredentials host account username

  case publishCredentials of
    Just (PublishCredentials { username: expectedUsername
                             , password: expectedPassword})
      | expectedUsername == username
      , expectedPassword == password -> do
        let
          rtmpShortName = wrap account
          rtmpStreamName = wrap streamName
          streamPublish = wrap { host
                               , protocol
                               , rtmpShortName
                               , rtmpStreamName
                               , username
                               }
        maybeStreamDetails <- getStreamDetails streamPublish

        case maybeStreamDetails of
          Nothing ->
            pure Nothing

          Just streamDetails -> do
            case findProfile streamDetails of
              Nothing -> do
                logInfo "StreamProfile not found" { streamDetails
                                                       , streamName }
                pure Nothing

              Just (SlotProfile { name: profileName }) -> do
                let
                  ingestKey = makeIngestKey profileName streamDetails
                logInfo "Starting WEBRTC" {canary}
                maybeStartStream <- case protocol of
                                      WebRTC -> do
                                        self <- Erl.self
                                        pure $ Just $ startStream ingestKey $ IngestInstanceSup.startLocalWebRTCIngest loadConfig canary ingestKey streamPublish streamDetails remoteAddress remotePort self
                                      Rtmp ->
                                        pure Nothing
                pure $ Just { streamDetails
                            , profileName
                            , startStream: maybeStartStream
                            , dataObjectSendMessage: IngestInstance.dataObjectSendMessage ingestKey
                            , dataObjectUpdate: IngestInstance.dataObjectUpdate ingestKey
                            }
    Just _ -> do
      logInfo "Authentication failed; invalid username / password" {username}
      pure Nothing

    Nothing -> do
      pure Nothing
  where
    findProfile streamDetails@{ slot: { profiles } } =
      find (\ (SlotProfile { rtmpStreamName: RtmpStreamName profileStreamName }) -> profileStreamName == streamName) profiles

    makeIngestKey profileName {role, slot: {id: slotId}} =
      IngestKey slotId role profileName

startStream :: IngestKey -> Effect (LocalResourceResp Server) -> Effect (Maybe StartStreamResult)
startStream ingestKey startFn = do
  maybeStarted <- startFn
  case maybeStarted of
    Right _ -> do
      workflowPid <- startWorkflow ingestKey
      pure $ Just { sourceInfo: sourceInfo
                  , stopStream: stopStream ingestKey
                  , workflowPid}
    Left error -> do
      logWarning "Attempt to start local RTMP ingest failed" {error}
      pure Nothing
  where
    sourceInfo foreignSourceInfo = do
      IngestInstance.setSourceInfo ingestKey (SourceDetails.foreignToSourceInfo foreignSourceInfo)

stopStream :: IngestKey -> Effect Unit
stopStream ingestKey =
  IngestInstance.stopIngest ingestKey

domain :: List Atom
domain = atom <$> (show Agent.Ingest : "Instance" : nil)

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo = Logger.info <<< Logger.traceMetadata domain

logWarning :: forall report. String -> { | report } -> Effect Unit
logWarning = Logger.warning <<< Logger.traceMetadata domain

startWorkflow :: IngestKey -> Effect Pid
startWorkflow ingestKey =
  startWorkflowImpl ingestKey
