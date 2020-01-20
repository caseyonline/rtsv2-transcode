module Rtsv2.Agents.IngestRtmpServer
       (
         startLink
       )
       where

import Prelude

import Data.Either (Either(..), hush)
import Data.Foldable (any)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Choice (left, right)
import Effect (Effect)
import Foreign (Foreign)
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen as Gen
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Names as Names
import Serf (Ip)
import Shared.LlnwApiTypes (AuthType, PublishCredentials, StreamConnection, StreamDetails, StreamIngestProtocol(..), StreamPublish, StreamAuth)
import Shared.Stream (StreamAndVariant(..))
import Simple.JSON as JSON
import SpudGun (bodyToJSON)
import SpudGun as SpudGun

type Callbacks
  = { ingestStarted :: StreamDetails -> String -> Effect (Either Unit StreamAndVariant)
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
    ingestStarted :: StreamDetails -> String -> Effect (Either Unit StreamAndVariant)
    ingestStarted streamDetails@{ role
                                , slot : {name : streamId, profiles}
                                } streamVariantId =
      case any (\{streamName: slotStreamName} -> slotStreamName == streamVariantId) profiles of
        true ->
          let
            streamAndVariant = StreamAndVariant (wrap streamId) (wrap streamVariantId)
          in
           IngestInstanceSup.startIngest streamDetails streamAndVariant
          <#> const (Right streamAndVariant)
        false ->
          pure $ Left unit

    ingestStopped :: StreamDetails -> String -> Effect Unit
    ingestStopped { role
                   , slot : {name : streamId}} streamVariantId = IngestInstance.stopIngest (StreamAndVariant (wrap streamId) (wrap streamVariantId))

    streamAuthType url host shortname = do
      restResult <- SpudGun.postJson (wrap url) ({ host
                                                 , protocol: Rtmp
                                                 , shortname} :: StreamConnection
                                                )
      pure $ hush (bodyToJSON restResult)

    streamAuth url host shortname username = do
      restResult <- SpudGun.postJson (wrap url) ({ host
                                                 , shortname
                                                 , username} :: StreamAuth
                                                )
      pure $ hush (bodyToJSON restResult)

    streamPublish url host shortname username streamName = do
      restResult <- SpudGun.postJson (wrap url) ({ host
                                                  , protocol: Rtmp
                                                  , shortname
                                                  , streamName
                                                  , username} :: StreamPublish
                                                )
      pure $ hush (bodyToJSON restResult)
