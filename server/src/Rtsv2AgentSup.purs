module Rtsv2AgentSup where

import Prelude

import Effect (Effect)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildSpec, SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..))
import Pinto.Sup as Sup
import Rtsv2Config as Rtsv2Config
import Rtsv2EdgeAgentSup as Rtsv2EdgeAgentSup
import Rtsv2IngestAgentSup as Rtsv2IngestAgentSup
import Rtsv2StreamRelayAgentSup as Rtsv2StreamRelayAgentSup
import Rtsv2IntraPoPAgent as Rtsv2IntraPoPAgent
import Rtsv2TransPoPAgent as Rtsv2TransPoPAgent

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
      makeSpec IngestAgent = Sup.buildChild
                             # Sup.childType Supervisor
                             # Sup.childId "ingestAgent"
                             # Sup.childStart Rtsv2IngestAgentSup.startLink unit
      makeSpec StreamRelayAgent = Sup.buildChild
                                  # Sup.childType Supervisor
                                  # Sup.childId "streamRelayAgent"
                                  # Sup.childStart Rtsv2StreamRelayAgentSup.startLink unit
      makeSpec IntraPoPAgent = Sup.buildChild
                               # Sup.childType Worker
                               # Sup.childId "intraPopAgent"
                               # Sup.childStart Rtsv2IntraPoPAgent.startLink {}
      makeSpec TransPoPAgent = Sup.buildChild
                               # Sup.childType Worker
                               # Sup.childId "transPopAgent"
                               # Sup.childStart Rtsv2TransPoPAgent.startLink {}
