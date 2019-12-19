module Rtsv2.IngestRtmpServer
       (
         startLink
       )
       where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush, note)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe)
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Utils as ErlUtils
import Foreign (Foreign, ForeignError(..))
import Pinto (ServerName(..))
import Pinto as Pinto
import Pinto.Gen as Gen
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.IngestAgent as IngestAgent
import Rtsv2.IngestAgentInstanceSup as IngestAgentInstanceSup
import Rtsv2.LlnwApiTypes (StreamPublish, StreamPublishProtocol(..), StreamDetails)
import Serf (Ip)
import Shared.Stream (StreamVariantId(..))
import Simple.JSON as JSON
import SpudGun as SpudGun

type Callbacks
  = { ingestStarted :: String -> String -> Effect Unit
    , ingestStopped :: String -> String -> Effect Unit
    , checkSlot :: String -> String -> String -> Effect (Maybe StreamDetails)
    }

isAvailable :: Effect Boolean
isAvailable = ErlUtils.isRegistered "ingestRtmpServer"

serverName :: ServerName Unit Unit
serverName = Local "ingestRtmpServer"

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink args = Gen.startLink serverName (init args) Gen.defaultHandleInfo

foreign import startServerImpl :: (Foreign -> Either Foreign Unit) -> Either Foreign Unit -> Ip -> Int -> Int -> Callbacks -> Effect (Either Foreign Unit)

init :: forall a. a -> Effect Unit
init _ = do
  interfaceIp <- Env.publicInterfaceIp
  {port, nbAcceptors} <- Config.rtmpIngestConfig
  {streamPublishUrl} <- Config.llnwApiConfig
  let
    callbacks = { ingestStarted
                , ingestStopped
                , checkSlot: checkSlot streamPublishUrl
                }
  _ <- startServerImpl Left (Right unit) interfaceIp port nbAcceptors callbacks
  pure $ unit
  where
    ingestStarted streamId streamVariantId = IngestAgentInstanceSup.startIngest (StreamVariantId streamId streamVariantId)

    ingestStopped streamId streamVariantId = IngestAgent.stopIngest (StreamVariantId streamId streamVariantId)

checkSlot :: String -> String -> String -> String -> Effect (Maybe StreamDetails)
checkSlot url host shortname streamName = do
  restResult <- SpudGun.post url (JSON.writeJSON ({host
                                                  , protocol: Rtmp
                                                  , shortname
                                                  , streamName} :: StreamPublish))
  let
    streamPublish = JSON.readJSON =<< lmap (\s -> (singleton (ForeignError s))) restResult
  pure $ hush streamPublish
