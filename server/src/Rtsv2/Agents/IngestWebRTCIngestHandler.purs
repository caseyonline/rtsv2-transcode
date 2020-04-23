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
import Rtsv2.Config as Config
import Shared.Agent as Agent
import Shared.LlnwApiTypes (PublishCredentials(..), SlotProfile(..), StreamAuth, StreamDetails, StreamIngestProtocol(..), StreamPublish(..))
import Shared.Stream (IngestKey(..), ProfileName, RtmpStreamName(..))
import SpudGun (JsonResponseError, bodyToJSON)
import SpudGun as SpudGun

foreign import startWorkflowImpl :: IngestKey -> Effect Pid

type AuthenticateResult = { streamDetails :: StreamDetails
                          , profileName :: ProfileName
                          , startStream :: Maybe (Effect (Maybe StartStreamResult))
                          }

type StartStreamResult = { sourceInfo :: Foreign -> Effect Unit
                         , workflowPid :: Pid
                         , stopStream :: Effect Unit
                         }

authenticate :: String -> StreamIngestProtocol -> String -> String -> String -> String -> String -> Int -> Effect (Maybe AuthenticateResult)
authenticate host protocol account username password streamName remoteAddress remotePort = do
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
                                        pure $ Just $ startStream ingestKey $ IngestInstanceSup.startIngest ingestKey streamPublish streamDetails remoteAddress remotePort self
                                      Rtmp ->
                                        pure Nothing
                pure $ Just { streamDetails
                            , profileName
                            , startStream: maybeStartStream
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


startStream :: IngestKey -> Effect (Maybe Unit) -> Effect (Maybe StartStreamResult)
startStream ingestKey startFn = do
  maybeStarted <- startFn
  case maybeStarted of
    Just _ -> do
      workflowPid <- startWorkflow ingestKey
      pure $ Just { sourceInfo: sourceInfo
                  , stopStream: stopStream ingestKey
                  , workflowPid}
    Nothing ->
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

startWorkflow :: IngestKey -> Effect Pid
startWorkflow ingestKey =
  startWorkflowImpl ingestKey
