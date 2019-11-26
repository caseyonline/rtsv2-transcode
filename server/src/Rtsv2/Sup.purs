module Rtsv2.Sup where

import Prelude
import Effect (Effect)
import Erl.Data.List (nil, (:))
import LocalPopState as PopState
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..), buildChild, buildSupervisor, childId, childStart, childType, supervisorChildren, supervisorStrategy)
import Pinto.Sup as Sup
import Rtsv2.AgentSup as AgentSup
import Rtsv2.Config as Config
import Rtsv2.Web as Web

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink "rtsv2sup" init

init :: Effect SupervisorSpec
init = do
  webPort <- Config.webPort
  let
    popDefinition =
      buildChild
      # childType Worker
      # childId "localPopState"
      # childStart PopState.startLink PopState.defaultStartArgs

    webServer =
      buildChild
      # childType Worker
      # childId "web"
      # childStart Web.startLink { webPort: webPort }

    agentSup =
      buildChild
      # childType Supervisor
      # childId "agentSup"
      # childStart AgentSup.startLink unit

  pure $ buildSupervisor
    # supervisorStrategy OneForOne
    # supervisorChildren
        ( popDefinition
            : agentSup
            : webServer
            : nil
        )
