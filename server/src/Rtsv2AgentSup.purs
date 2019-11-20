module Rtsv2AgentSup where

import Effect
import Erl.Data.List
import Prelude

import Pinto as Pinto
import Pinto.Sup (startLink, startChild) as Sup
import Pinto.Sup
import Rtsv2Web as Rtsv2Web
import Rtsv2Config as Rtsv2Config
import Rtsv2EdgeAgentSup as Rtsv2EdgeAgentSup

startLink :: Unit -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink "agentSup" init

init :: Effect SupervisorSpec
init = do
  pure $ buildSupervisor
                # supervisorStrategy OneForOne
                # supervisorChildren nil

start_edge =
  Sup.startChild "agentSup" $ buildChild
                              # childType Supervisor
                              # childId "edgeAgent"
                              # childStart Rtsv2EdgeAgentSup.startLink unit
