module Rtsv2.IngestRtmpServer
       (
         startLink
       )
       where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Erl.Utils as ErlUtils
import Foreign (Foreign)
import Pinto (ServerName(..))
import Pinto as Pinto
import Pinto.Gen as Gen
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.IngestAgent as IngestAgent
import Rtsv2.IngestAgentInstanceSup as IngestAgentInstanceSup
import Serf (Ip)
import Shared.Stream (StreamVariantId(..))

type Callbacks
  = { ingestStarted :: String -> String -> Effect Unit
    , ingestStopped :: String -> String -> Effect Unit
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
  let
    ingestStarted streamId streamVariantId = IngestAgentInstanceSup.startIngest (StreamVariantId streamId streamVariantId)

    ingestStopped streamId streamVariantId = IngestAgent.stopIngest (StreamVariantId streamId streamVariantId)
    
    callbacks = { ingestStarted
                , ingestStopped
                }
                
  interfaceIp <- Env.publicInterfaceIp
  {port, nbAcceptors} <- Config.rtmpIngestConfig
  _ <- startServerImpl Left (Right unit) interfaceIp port nbAcceptors callbacks
  pure $ unit

