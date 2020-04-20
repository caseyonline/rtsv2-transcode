 module Rtsv2.Agents.EgestSup
  ( isAgentAvailable
  , startLink
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (catMaybes, nil, (:))
import Erl.ModuleName (NativeModuleName(..))
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), buildChild, childId, childStart, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Config as Config
import Rtsv2.Names as Names

isAgentAvailable :: Effect Boolean
isAgentAvailable = Pinto.isRegistered serverName

serverName :: SupervisorName
serverName = Names.egestSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

init :: Effect Sup.SupervisorSpec
init = do
  agentSpecs <- sequence $ (instanceSup : mediaGateway : nil)

  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.OneForAll
    # (Sup.supervisorChildren $ catMaybes agentSpecs)

  where
    instanceSup = pure $ Just $ buildChild
                                # childType Supervisor
                                # childId "egestAgent"
                                # childStart EgestInstanceSup.startLink unit

    mediaGateway = do
      { useMediaGateway } <- Config.featureFlags
      pure $ if useMediaGateway then
               Just $ buildChild
                      # childType (NativeWorker (NativeModuleName (atom "rtsv2_media_gateway_api")) (atom "start_link") nil)
                      # childId "mediaGateway"
             else
               Nothing
