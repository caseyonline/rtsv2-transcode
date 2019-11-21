module Rtsv2Sup where

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Prelude

import Pinto as Pinto
import Pinto.Sup ( SupervisorChildType(..)
                 , SupervisorSpec
                 , SupervisorStrategy(..)
                 , buildChild
                 , buildSupervisor
                 , childId
                 , childStart
                 , childType
                 , supervisorChildren
                 , supervisorStrategy
                 )
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
        ( agentSup
            : (webServer webPort)
            : nil
        )
  where
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
