module Rtsv2.Agents.IngestRtmpServer
       (
         startLink
       )
       where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Foldable (any)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe)
import Effect (Effect)
import Foreign (Foreign, ForeignError(..))
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen as Gen
import Rtsv2.Names as Names
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.LlnwApiTypes (AuthType, PublishCredentials, StreamConnection, StreamDetails, StreamIngestProtocol(..), StreamPublish, StreamAuth)
import Serf (Ip)
import Shared.Stream (StreamVariantId(..))
import Simple.JSON as JSON
import SpudGun as SpudGun

type Callbacks
  = { ingestStarted :: StreamDetails -> String -> Effect Boolean
    , ingestStopped :: StreamDetails -> String -> Effect Unit
    , streamAuthType :: String -> String -> Effect (Maybe AuthType)
    , streamAuth ::  String -> String -> String -> Effect (Maybe PublishCredentials)
    , streamPublish :: String -> String -> String -> String -> Effect (Maybe StreamDetails)
    }

isAvailable :: Effect Boolean
isAvailable = Names.isRegistered serverName

serverName :: ServerName State Unit
serverName = Names.ingestRtmpServerName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink args = Gen.startLink serverName (init args) Gen.defaultHandleInfo

foreign import startServerImpl :: (Foreign -> Either Foreign Unit) -> Either Foreign Unit -> Ip -> Int -> Int -> Callbacks -> Effect (Either Foreign Unit)

type State =
  {
  }


init :: forall a. a -> Effect State
init _ = do
  interfaceIp <- Env.publicInterfaceIp
  {port, nbAcceptors} <- Config.rtmpIngestConfig
  {streamAuthTypeUrl, streamAuthUrl, streamPublishUrl} <- Config.llnwApiConfig
  let
    callbacks = { ingestStarted
                , ingestStopped
                , streamAuthType: streamAuthType streamAuthTypeUrl
                , streamAuth: streamAuth streamAuthUrl
                , streamPublish: streamPublish streamPublishUrl
                }
  _ <- startServerImpl Left (Right unit) interfaceIp port nbAcceptors callbacks
  pure $ {}
  where
    ingestStarted { role
                  , slot : {name : streamId, profiles}
                  } streamVariantId =
      case any (\{streamName: slotStreamName} -> slotStreamName == streamVariantId) profiles of
        true ->
          IngestInstanceSup.startIngest (StreamVariantId streamId streamVariantId)
          <#> const true
        false ->
          pure $ false

    ingestStopped { role
                   , slot : {name : streamId}} streamVariantId = IngestInstance.stopIngest (StreamVariantId streamId streamVariantId)

    streamAuthType url host shortname = do
      restResult <- SpudGun.post url (JSON.writeJSON ({host
                                                      , protocol: Rtmp
                                                      , shortname} :: StreamConnection))
      let
        authType = JSON.readJSON =<< lmap (\s -> (singleton (ForeignError s))) restResult
      pure $ hush authType

    streamAuth url host shortname username = do
      restResult <- SpudGun.post url (JSON.writeJSON ({host
                                                      , shortname
                                                      , username} :: StreamAuth))
      let
        publishCredentials = JSON.readJSON =<< lmap (\s -> (singleton (ForeignError s))) restResult
      pure $ hush publishCredentials

    streamPublish url host shortname username streamName = do
      restResult <- SpudGun.post url (JSON.writeJSON ({host
                                                      , protocol: Rtmp
                                                      , shortname
                                                      , streamName
                                                      , username} :: StreamPublish))
      let
        streamDetails = JSON.readJSON =<< lmap (\s -> (singleton (ForeignError s))) restResult
      pure $ hush streamDetails


