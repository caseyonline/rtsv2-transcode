module Rtsv2AgentSup where

import Effect
import Erl.Data.List
import Pinto.Sup
import Prelude

import Agents (Agent(..))
import Pinto as Pinto
import Pinto.Sup (startLink, startChild) as Sup
import Rtsv2Config as Rtsv2Config
import Rtsv2EdgeAgentSup as Rtsv2EdgeAgentSup
import Rtsv2Web as Rtsv2Web

startLink :: Unit -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink "agentSup" init

init :: Effect SupervisorSpec
init = do
  initialAgents <- Rtsv2Config.initialAgents

  pure $ buildSupervisor
                # supervisorStrategy OneForOne
                # supervisorChildren (makeSpec <$> initialAgents)
    where
      makeSpec :: Agent -> SupervisorChildSpec
      makeSpec EdgeAgent = buildChild
                              # childType Supervisor
                              # childId "edgeAgent"
                              # childStart Rtsv2EdgeAgentSup.startLink nil
