 module Rtsv2.Agents.EgestSup
  ( isAgentAvailable
  , startLink
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (catMaybes, nil, (:))
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStart, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.EgestRtmpServer as EgestRtmpServer
import Rtsv2.Agents.EgestStats as EgestStats
import Rtsv2.Config (MediaGatewayFlag(..))
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Rtsv2.Types (extractAddress)

isAgentAvailable :: Effect Boolean
isAgentAvailable = Pinto.isRegistered serverName

serverName :: SupervisorName
serverName = Names.egestSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

init :: Effect Sup.SupervisorSpec
init = do
  agentSpecs <- sequence $ (instanceSup : mediaGateway : rtmpServer : stats : nil)

  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.OneForAll
    # (Sup.supervisorChildren $ catMaybes agentSpecs)

  where
    instanceSup = pure $ Just $ buildChild
                                # childType Supervisor
                                # childId "egestAgent"
                                # childStart EgestInstanceSup.startLink unit

    rtmpServer = pure $ Just $ buildChild
                  # childType Worker
                  # childId "egestRtmpServer"
                  # childStart EgestRtmpServer.startLink unit
                  # childRestart Transient

    stats = pure $ Just $ buildChild
            # childType Worker
            # childId "egestStatsServer"
            # childStart EgestStats.startLink unit

    mediaGateway = do
      { mediaGateway: mediaGatewayFlag } <- Config.featureFlags
      thisServer <- PoPDefinition.getThisServer
      pure $
        case mediaGatewayFlag of
          Off ->
            Nothing
          _ ->
            let
              serverAddress = (unwrap $ extractAddress thisServer) :: String
              networkCapacityBitsPerSecond = 1000 * (unwrap $ (unwrap thisServer).maxNetworkCapacity)
              transmitQueueCount = ((unwrap thisServer).transmitQueueCount) :: Int
              receiveQueueCount = ((unwrap thisServer).receiveQueueCount) :: Int
              maxTransmitQueueCapacity = networkCapacityBitsPerSecond / transmitQueueCount
              transmitQueueCapacityLessHeadroom = (maxTransmitQueueCapacity * 80) / 100
              args
                = (unsafeToForeign $ serverAddress)
                : (unsafeToForeign $ transmitQueueCount)
                : (unsafeToForeign $ receiveQueueCount)
                : (unsafeToForeign $ transmitQueueCapacityLessHeadroom)
                : (unsafeToForeign mediaGatewayFlag)
                : nil
            in
              Just $ buildChild
                    # childType (NativeWorker (NativeModuleName (atom "rtsv2_media_gateway_api")) (atom "start_link") args)
                    # childId "mediaGateway"
