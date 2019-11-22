module Rtsv2AgentSup where

import Prelude

import Effect (Effect)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildSpec, SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..))
import Pinto.Sup as Sup
import Rtsv2Config as Rtsv2Config
import Rtsv2EdgeAgentSup as Rtsv2EdgeAgentSup
import Shared.Agents (Agent(..))

startLink :: Unit -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink "agentSup" init

init :: Effect SupervisorSpec
init = do
  initialAgents <- Rtsv2Config.configuredAgents

  pure $ Sup.buildSupervisor
                # Sup.supervisorStrategy OneForOne
                # Sup.supervisorChildren (makeSpec <$> initialAgents)
    where
      makeSpec :: Agent -> SupervisorChildSpec
      makeSpec EdgeAgent = Sup.buildChild
                              # Sup.childType Supervisor
                              # Sup.childId "edgeAgent"
                              # Sup.childStart Rtsv2EdgeAgentSup.startLink unit
