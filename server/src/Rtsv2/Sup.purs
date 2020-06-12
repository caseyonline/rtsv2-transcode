module Rtsv2.Sup where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Pinto (ServerName(..))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..), buildChild, buildSupervisor, childId, childRestart, childStart, childType, supervisorChildren, supervisorStrategy)
import Pinto.Sup as Sup
import Rtsv2.ActiveSup as ActiveSup
import Rtsv2.Alerts as Alerts
import Rtsv2.Config as Config
import Rtsv2.Load as Load
import Rtsv2.NodeManager as NodeManager
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Web as Web

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink (Local (atom "rtsv2sup")) init

init :: Effect SupervisorSpec
init = do
  webConfig <- Config.webConfig
  popDefinitionConfig <- Config.popDefinitionConfig
  pure $ buildSupervisor
    # supervisorStrategy OneForOne
    # supervisorChildren
        ( popDefinition popDefinitionConfig
          : nodeManager
          : load
          : alerts
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

    alerts =
      buildChild
      # childType Worker
      # childId "alerts"
      # childStart Alerts.startLink unit

    nodeManager =
      buildChild
      # childType Worker
      # childId "nodeManager"
      # childStart NodeManager.startLink { activeSupStartLink: ActiveSup.startLink
                                         }
