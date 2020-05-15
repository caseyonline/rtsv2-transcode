module Rtsv2.Sup where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Pinto (ServerName(..))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..), buildChild, buildSupervisor, childId, childStart, childType, supervisorChildren, supervisorStrategy)
import Pinto.Sup as Sup
import Rtsv2.Agents.AgentSup as AgentSup
import Rtsv2.Config as Config
import Rtsv2.DataObject as DataObject
import Rtsv2.Load as Load
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.NodeManager as NodeManager
import Rtsv2.Web as Web

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink (Local (atom "rtsv2sup")) init

foreign import traceDebugImpl :: Effect Unit

init :: Effect SupervisorSpec
init = do
  webConfig <- Config.webConfig
  popDefinitionConfig <- Config.popDefinitionConfig
--  _ <- traceDebugImpl
  pure $ buildSupervisor
    # supervisorStrategy OneForOne
    # supervisorChildren
        ( popDefinition popDefinitionConfig
          : nodeManager
          : agentSup
          : load
          : dataObject
          : webServer webConfig
          : nil
        )
  where
    popDefinition popDefinitionConfig =
      buildChild
      # childType Worker
      # childId "popDefinition"
      # childStart PoPDefinition.startLink popDefinitionConfig

    webServer webConfig =
      buildChild
      # childType Worker
      # childId "web"
      # childStart Web.startLink webConfig

    load =
      buildChild
      # childType Worker
      # childId "load"
      # childStart Load.startLink unit

    nodeManager =
      buildChild
      # childType Worker
      # childId "nodeManager"
      # childStart NodeManager.startLink unit

    dataObject =
      buildChild
      # childType Worker
      # childId "dataObject"
      # childStart DataObject.startLink unit

    agentSup =
      buildChild
      # childType Supervisor
      # childId "agentSup"
      # childStart AgentSup.startLink unit
