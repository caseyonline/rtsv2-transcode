 module Rtsv2.Agents.EgestSup
  ( isAgentAvailable
  , startLink
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (catMaybes, nil, singleton, (:))
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), buildChild, childId, childStart, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
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
      thisServer <- PoPDefinition.getThisServer
      pure $ if useMediaGateway then
               let
                 args = singleton $ unsafeToForeign $ extractAddress thisServer
               in
                 Just $ buildChild
                        # childType (NativeWorker (NativeModuleName (atom "rtsv2_media_gateway_api")) (atom "start_link") args)
                        # childId "mediaGateway"
             else
               Nothing
