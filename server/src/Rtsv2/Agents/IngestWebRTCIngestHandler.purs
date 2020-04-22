module Rtsv2.Agents.IngestWebRTCIngestHandler
       (
         authenticate
       , publishStream
       , stopStream
       )
       where

import Prelude

import Data.Either (Either(..), hush)
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Process.Raw (Pid)
import Erl.Utils (self)
import Foreign (Foreign)
import Logger (Logger, spy)
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

authenticate :: String -> String -> String -> String -> Effect (Maybe Unit)
authenticate host account username password = do
  maybePublishCredentials <- getPublishCredentials host account username

  case maybePublishCredentials of
    Just (PublishCredentials { username: expectedUsername
                             , password: expectedPassword})
      | expectedUsername == username
        , expectedPassword == password -> do
        pure $ Just unit
    _ ->
      pure Nothing

publishStream :: String -> String -> String -> String -> Int -> String -> Effect (Maybe { streamDetails :: StreamDetails
                                                                                        , profileName :: ProfileName
                                                                                        , sourceInfo :: Foreign -> Effect Unit
                                                                                        , workflowPid :: Pid
                                                                                        } )
publishStream host account username remoteAddress remotePort streamName = do
  let
    rtmpShortName = wrap account
    rtmpStreamName = wrap streamName
    streamPublish = wrap { host
                         , protocol: WebRTC
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
          self <- self
          -- TODO - if ingest running, this just crashes
          IngestInstanceSup.startIngest ingestKey streamPublish streamDetails remoteAddress remotePort self
          workflowPid <- startWorkflow ingestKey
          pure $ Just {streamDetails, profileName, sourceInfo: sourceInfo ingestKey, workflowPid}
  where
    findProfile streamDetails@{ slot: { profiles } } =
      find (\ (SlotProfile { rtmpStreamName: RtmpStreamName profileStreamName }) -> profileStreamName == streamName) profiles

    makeIngestKey profileName {role, slot: {id: slotId}} =
      IngestKey slotId role profileName

    sourceInfo ingestKey foreignSourceInfo = do
      IngestInstance.setSourceInfo ingestKey (SourceDetails.foreignToSourceInfo foreignSourceInfo)

stopStream :: IngestKey -> Effect Unit
stopStream ingestKey =
  -- workflow is still running, we need to stop it - can return it from publishStream and have it passed into stopStream
  IngestInstance.stopIngest ingestKey

getPublishCredentials :: String -> String -> String -> Effect (Maybe PublishCredentials)
getPublishCredentials host rtmpShortName username = do
  {streamAuthUrl: url} <- Config.llnwApiConfig
  restResult <- SpudGun.postJson (wrap url) (wrap { host
                                                  , rtmpShortName: wrap rtmpShortName
                                                  , username} :: StreamAuth)
  pure $ hush $ bodyToJSON restResult

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
