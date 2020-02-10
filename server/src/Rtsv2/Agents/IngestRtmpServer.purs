module Rtsv2.Agents.IngestRtmpServer
       (
         startLink
       )
       where

import Prelude

import Data.Either (Either(..), hush)
import Data.Foldable (any)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, mkFn2, mkFn3)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Effect (Effect)
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
import Shared.Stream (StreamAndVariant(..))
import SpudGun (bodyToJSON)
import SpudGun as SpudGun

type Callbacks
  = { ingestStarted :: Fn3 StreamDetails String Pid (Effect (Either Unit StreamAndVariant))
    , ingestStopped :: Fn2 StreamDetails String (Effect Unit)
    , streamAuthType :: Fn2 String String (Effect (Maybe AuthType))
    , streamAuth ::  Fn3 String String String (Effect (Maybe PublishCredentials))
    , streamPublish :: Fn3 String String String (Fn1 String (Effect (Maybe StreamDetails)))
    , clientMetadata :: Fn2 StreamAndVariant Foreign (Effect Unit)
    , sourceInfo :: Fn2 StreamAndVariant Foreign (Effect Unit)
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
    callbacks = { ingestStarted: mkFn3 ingestStarted
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

    clientMetadata streamAndVariant foreignMetadata = do
      IngestInstance.setClientMetadata streamAndVariant (Rtmp.foreignToMetadata foreignMetadata)
      pure unit

    sourceInfo streamAndVariant foreignSourceInfo = do
      IngestInstance.setSourceInfo streamAndVariant (SourceDetails.foreignToSourceInfo (spy "foreign" foreignSourceInfo))
      pure unit
