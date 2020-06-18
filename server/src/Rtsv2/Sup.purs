module Rtsv2.Sup where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Pinto (ServerName(..), SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..), buildChild, buildSupervisor, childId, childStart, childType, supervisorChildren, supervisorStrategy)
import Pinto.Sup as Sup
import Rtsv2.ActiveSup as ActiveSup
import Rtsv2.Alerts as Alerts
import Rtsv2.Config as Config
import Rtsv2.Load as Load
import Rtsv2.NodeManager as NodeManager
import Rtsv2.PoPDefinition as PoPDefinition

serverName :: SupervisorName
serverName = (Local (atom "rtsv2sup"))

startLink :: Effect Pinto.StartLinkResult
startLink = Sup.startLink serverName init

stop :: Effect Unit
stop = Sup.stop serverName

init :: Effect SupervisorSpec
init = do
  popDefinitionConfig <- Config.popDefinitionConfig
  pure $ buildSupervisor
    # supervisorStrategy OneForOne
    # supervisorChildren
        ( popDefinition popDefinitionConfig
          : nodeManager
          : load
          : alerts
          : nil
        )
  where
    popDefinition popDefinitionConfig =
      buildChild
      # childType Worker
      # childId "popDefinition"
      # childStart PoPDefinition.startLink popDefinitionConfig

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
                                         , activeSupStop: ActiveSup.stop
                                         }
