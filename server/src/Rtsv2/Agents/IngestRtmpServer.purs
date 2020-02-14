module Rtsv2.Agents.IngestRtmpServer
       (
         startLink
       )
       where

import Prelude

import Data.Either (Either(..), hush)
import Data.Foldable (any)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn6, mkFn2, mkFn3, mkFn6)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Data.Tuple (Tuple2, tuple2)
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Logger (spy)
import Media.Rtmp as Rtmp
import Media.SourceDetails as SourceDetails
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen as Gen
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Names as Names
import Rtsv2.Utils (crashIfLeft)
import Serf (Ip)
import Shared.LlnwApiTypes (AuthType, PublishCredentials, StreamConnection, StreamDetails, StreamIngestProtocol(..), StreamPublish, StreamAuth)
import Shared.Stream (IngestKey(..), StreamAndVariant(..), StreamRole(..))
import SpudGun (bodyToJSON)
import SpudGun as SpudGun

type Callbacks
  = { ingestStarted :: Fn6 StreamPublish StreamDetails String String Int Pid (Effect (Either Unit IngestKey))
    , ingestStopped :: Fn2 StreamDetails String (Effect Unit)
    , streamAuthType :: Fn2 String String (Effect (Maybe AuthType))
    , streamAuth ::  Fn3 String String String (Effect (Maybe PublishCredentials))
    , streamPublish :: Fn3 String String String (Fn1 String (Effect (Maybe (Tuple2 StreamPublish StreamDetails))))
    , clientMetadata :: Fn2 IngestKey Foreign (Effect Unit)
    , sourceInfo :: Fn2 IngestKey Foreign (Effect Unit)
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
    callbacks :: Callbacks
    callbacks = { ingestStarted: mkFn6 ingestStarted
                , ingestStopped: mkFn2 ingestStopped
                , streamAuthType: mkFn2 (streamAuthType streamAuthTypeUrl)
                , streamAuth: mkFn3 (streamAuth streamAuthUrl)
                , streamPublish: mkFn3 (streamPublish streamPublishUrl)
                , clientMetadata: mkFn2 clientMetadata
                , sourceInfo: mkFn2 sourceInfo
                }
  crashIfLeft =<< startServerImpl Left (Right unit) interfaceIp port nbAcceptors callbacks
  pure $ {}
  where
    ingestStarted :: StreamPublish -> StreamDetails -> String -> String -> Int -> Pid -> Effect (Either Unit IngestKey)
    ingestStarted streamPublishInfo streamDetails@{ role
                                                  , slot : {name : streamId, profiles}
                                                  } streamVariantId remoteAddress remotePort pid =
      case any (\{streamName: slotStreamName} -> slotStreamName == streamVariantId) profiles of
        true ->
          let
            ingestKey = IngestKey (wrap streamId) role (wrap streamVariantId)
          in
           IngestInstanceSup.startIngest ingestKey streamPublishInfo streamDetails remoteAddress remotePort pid
          <#> const (Right ingestKey)
        false ->
          pure $ Left unit

    ingestStopped :: StreamDetails -> String -> Effect Unit
    ingestStopped { role
                   , slot : {name : streamId}} streamVariantId = IngestInstance.stopIngest (IngestKey (wrap streamId) role (wrap streamVariantId))

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
      let
        streamPublishPayload :: StreamPublish
        streamPublishPayload = { host
                               , protocol: Rtmp
                               , shortname
                               , streamName
                               , username}
      restResult <- SpudGun.postJson (wrap (spy "publish url" url)) (spy "publish body" streamPublishPayload)
      pure $ (tuple2 streamPublishPayload) <$> hush (spy "publish parse" (bodyToJSON (spy "publish result" restResult)))

    clientMetadata ingestKey foreignMetadata = do
      IngestInstance.setClientMetadata ingestKey (Rtmp.foreignToMetadata foreignMetadata)

    sourceInfo ingestKey foreignSourceInfo = do

      IngestInstance.setSourceInfo ingestKey (SourceDetails.foreignToSourceInfo (spy "foreign" foreignSourceInfo))
