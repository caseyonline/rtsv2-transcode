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
import Logger (Logger)
import Logger as Logger
import Media.SourceDetails as SourceDetails
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.DataObject as DO
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.LlnwApiTypes (PublishCredentials(..), SlotProfile(..), StreamAuth, StreamDetails, StreamIngestProtocol(..), StreamPublish(..))
import Shared.Rtsv2.Stream (IngestKey(..), ProfileName, RtmpStreamName(..))
import Shared.Rtsv2.Types (ResourceResp, Server(..))
import SpudGun (JsonResponseError, bodyToJSON)
import SpudGun as SpudGun

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

authenticate :: LoadConfig -> String -> StreamIngestProtocol -> String -> String -> String -> String -> String -> Int -> Effect (Maybe AuthenticateResult)
authenticate loadConfig host protocol account username password streamName remoteAddress remotePort = do
  publishCredentials <- getPublishCredentials host account username

  case publishCredentials of
    Right (PublishCredentials { username: expectedUsername
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
          Left error -> do
            _ <- logInfo "StreamPublish rejected" {reason: error}
            pure Nothing

          Right streamDetails -> do
            case findProfile streamDetails of
              Nothing -> do
                _ <- logInfo "StreamProfile not found" { streamDetails
                                                       , streamName }
                pure Nothing

              Just (SlotProfile { name: profileName }) -> do
                let
                  ingestKey = makeIngestKey profileName streamDetails
                maybeStartStream <- case protocol of
                                      WebRTC -> do
                                        self <- Erl.self
                                        pure $ Just $ startStream ingestKey $ IngestInstanceSup.startLocalWebRTCIngest loadConfig ingestKey streamPublish streamDetails remoteAddress remotePort self
                                      Rtmp ->
                                        pure Nothing
                pure $ Just { streamDetails
                            , profileName
                            , startStream: maybeStartStream
                            , dataObjectSendMessage: IngestInstance.dataObjectSendMessage ingestKey
                            , dataObjectUpdate: IngestInstance.dataObjectUpdate ingestKey
                            }
    Right _ -> do
      _ <- logInfo "Authentication failed; invalid username / password" {username}
      pure Nothing

    Left error -> do
      _ <- logInfo "Authentication error" {reason: error}
      pure Nothing
  where
    findProfile streamDetails@{ slot: { profiles } } =
      find (\ (SlotProfile { rtmpStreamName: RtmpStreamName profileStreamName }) -> profileStreamName == streamName) profiles

    makeIngestKey profileName {role, slot: {id: slotId}} =
      IngestKey slotId role profileName


startStream :: IngestKey -> Effect (ResourceResp Server) -> Effect (Maybe StartStreamResult)
startStream ingestKey startFn = do
  maybeStarted <- startFn
  case maybeStarted of
    Right _ -> do
      workflowPid <- startWorkflow ingestKey
      pure $ Just { sourceInfo: sourceInfo
                  , stopStream: stopStream ingestKey
                  , workflowPid}
    Left error -> do
      _ <- logWarning "Attempt to start local RTMP ingest failed" {error}
      pure Nothing
  where
    sourceInfo foreignSourceInfo = do
      IngestInstance.setSourceInfo ingestKey (SourceDetails.foreignToSourceInfo foreignSourceInfo)

stopStream :: IngestKey -> Effect Unit
stopStream ingestKey =
  IngestInstance.stopIngest ingestKey

getPublishCredentials :: String -> String -> String -> Effect (Either JsonResponseError PublishCredentials)
getPublishCredentials host rtmpShortName username = do
  {streamAuthUrl: url} <- Config.llnwApiConfig
  restResult <- SpudGun.postJson (wrap url) (wrap { host
                                                  , rtmpShortName: wrap rtmpShortName
                                                  , username} :: StreamAuth)
  pure $ bodyToJSON restResult

getStreamDetails :: StreamPublish -> Effect (Either JsonResponseError StreamDetails)
getStreamDetails streamPublish@(StreamPublish {rtmpStreamName}) = do
  {streamPublishUrl: url} <- Config.llnwApiConfig
  restResult <- SpudGun.postJson (wrap url) streamPublish
  pure $ bodyToJSON restResult

domain :: List Atom
domain = atom <$> (show Agent.Ingest : "Instance" : nil)

logInfo :: forall a. Logger (Record a)
logInfo = Logger.doLog domain Logger.info

logWarning :: forall a. Logger (Record a)
logWarning = Logger.doLog domain Logger.warning

startWorkflow :: IngestKey -> Effect Pid
startWorkflow ingestKey =
  startWorkflowImpl ingestKey
