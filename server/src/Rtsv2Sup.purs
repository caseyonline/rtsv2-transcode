module Rtsv2Sup where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import LocalPopState as PopState
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..), buildChild, buildSupervisor, childId, childStart, childType, supervisorChildren, supervisorStrategy)
import Pinto.Sup as Sup
import Rtsv2AgentSup as Rtsv2AgentSup
import Rtsv2Config as Rtsv2Config
import Rtsv2Web as Rtsv2Web

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink "rtsv2sup" init

init :: Effect SupervisorSpec
init = do
  webPort <- Rtsv2Config.webPort
  pure $ buildSupervisor
    # supervisorStrategy OneForOne
    # supervisorChildren
        ( globalState
            : agentSup
            : (webServer webPort)
            : nil
        )
  where
  globalState =
          buildChild
      # childType Worker
      # childId "globalState"
      # childStart PopState.startLink {}

  webServer port =
    buildChild
      # childType Worker
      # childId "web"
      # childStart Rtsv2Web.startLink { webPort: port }

  agentSup =
    buildChild
      # childType Supervisor
      # childId "agentSup"
      # childStart Rtsv2AgentSup.startLink unit
