module Rtsv2.Sup where

import Prelude
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..), buildChild, buildSupervisor, childId, childStart, childType, supervisorChildren, supervisorStrategy)
import Pinto.Sup as Sup
import Rtsv2.Agents.AgentSup as AgentSup
import Rtsv2.Config as Config
import Rtsv2.Load as Load
import Rtsv2.Web as Web
import Rtsv2.PoPDefinition as PoPDefinition

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink "rtsv2sup" init

init :: Effect SupervisorSpec
init = do
  webConfig <- Config.webConfig
  popDefinitionConfig <- Config.popDefinitionConfig
  let
    popDefinition =
      buildChild
      # childType Worker
      # childId "popDefinition"
      # childStart PoPDefinition.startLink popDefinitionConfig

    webServer =
      buildChild
      # childType Worker
      # childId "web"
      # childStart Web.startLink webConfig

    load =
      buildChild
      # childType Worker
      # childId "load"
      # childStart Load.startLink unit


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
            : load
            : nil
        )
