module Rtsv2Sup where

import Effect
import Erl.Data.List
import Prelude

import Pinto as Pinto
import Pinto.Sup (startLink) as Sup
import Pinto.Sup
import Rtsv2Web as Rtsv2Web
import Rtsv2Config as Rtsv2Config
import Rtsv2AgentSup as Rtsv2AgentSup

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink "rtsv2sup" init

init :: Effect SupervisorSpec
init = do
  webPort <- Rtsv2Config.webPort
  pure $ buildSupervisor
                # supervisorStrategy OneForOne
                # supervisorChildren ( (webServer webPort)
                                       : agentSup
                                       : nil)
  where
    webServer port = buildChild
                     # childType Worker
                     # childId "web"
                     # childStart Rtsv2Web.startLink { webPort: port }

    agentSup = buildChild
              # childType Supervisor
              # childId "agentSup"
              # childStart Rtsv2AgentSup.startLink unit
