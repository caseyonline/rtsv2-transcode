module Rtsv2.Agents.IngestRtmpServer
       (
         startLink
       )
       where

import Prelude

import Data.Either (Either(..), hush)
import Data.Foldable (any)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Logger (spy)
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
import Shared.Types.Media.Types.Rtmp (foreignToMetadata)
import SpudGun (bodyToJSON)
import SpudGun as SpudGun

type Callbacks
  = { ingestStarted :: StreamDetails -> String -> Pid -> Effect (Either Unit StreamAndVariant)
    , ingestStopped :: StreamDetails -> String -> Effect Unit
    , streamAuthType :: String -> String -> Effect (Maybe AuthType)
    , streamAuth ::  String -> String -> String -> Effect (Maybe PublishCredentials)
    , streamPublish :: String -> String -> String -> String -> Effect (Maybe StreamDetails)
    , clientMetadata :: StreamAndVariant -> Foreign -> Effect Unit
    }

isAvailable :: Effect Boolean
isAvailable = Pinto.isRegistered serverName

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
                , clientMetadata
                }
  _ <- startServerImpl Left (Right unit) interfaceIp port nbAcceptors callbacks
  pure $ {}
  where
    ingestStarted :: StreamDetails -> String -> Pid -> Effect (Either Unit StreamAndVariant)
    ingestStarted streamDetails@{ role
                                , slot : {name : streamId, profiles}
                                } streamVariantId pid =
      case any (\{streamName: slotStreamName} -> slotStreamName == streamVariantId) profiles of
        true ->
          let
            streamAndVariant = StreamAndVariant (wrap streamId) (wrap streamVariantId)
          in
           IngestInstanceSup.startIngest streamDetails streamAndVariant pid
          <#> const (Right streamAndVariant)
        false ->
          pure $ Left unit

    ingestStopped :: StreamDetails -> String -> Effect Unit
    ingestStopped { role
                   , slot : {name : streamId}} streamVariantId = IngestInstance.stopIngest (StreamAndVariant (wrap streamId) (wrap streamVariantId))

    streamAuthType url host shortname = do
      restResult <- SpudGun.postJson (wrap (spy "authtype url" url)) (spy "authtype body" { host
                                                                                          , protocol: Rtmp
                                                                                          , shortname} :: StreamConnection
                                                                     )
      pure $ hush (bodyToJSON (spy "authtype result" restResult))

    streamAuth url host shortname username = do
      restResult <- SpudGun.postJson (wrap (spy "auth url" url)) (spy "auth body" { host
                                                                                  , shortname
                                                                                  , username} :: StreamAuth
                                                                 )
      pure $ hush (bodyToJSON (spy "auth result" restResult))

    streamPublish url host shortname username streamName = do
      restResult <- SpudGun.postJson (wrap (spy "publish url" url)) (spy "publish body" { host
                                                                                        , protocol: Rtmp
                                                                                        , shortname
                                                                                        , streamName
                                                                                        , username} :: StreamPublish
                                                                    )
      pure $ hush (spy "publish parse" (bodyToJSON (spy "publish result" restResult)))

    clientMetadata streamAndVariant metadata = do
      _ <- IngestInstance.setClientMetadata streamAndVariant (foreignToMetadata metadata)
      pure unit
