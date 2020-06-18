module Rtsv2.ActiveSup where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Pinto (ServerName(..))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..), buildChild, buildSupervisor, childId, childStart, childType, supervisorChildren, supervisorStrategy)
import Pinto.Sup as Sup
import Rtsv2.Agents.AgentSup as AgentSup
import Rtsv2.DataObject as DataObject
import Rtsv2.LlnwApi as LlnwApi
import Rtsv2.Types (AgentSupStartArgs)
import Rtsv2.Web as Web

serverName :: forall state msg. ServerName state msg
serverName = Local (atom "rtsv2ActiveSup")

startLink :: AgentSupStartArgs -> Effect Pinto.StartLinkResult
startLink args = Sup.startLink serverName (init args)

stop :: Effect Unit
stop = Sup.stop serverName

init :: AgentSupStartArgs -> Effect SupervisorSpec
init args = do
  pure $ buildSupervisor
    # supervisorStrategy OneForOne
    # supervisorChildren
    ( agentSup
      : dataObject
      : llnwApi
      : webServer
      : nil
    )
  where
    dataObject =
      buildChild
      # childType Worker
      # childId "dataObject"
      # childStart DataObject.startLink unit

    llnwApi =
      buildChild
      # childType Worker
      # childId "llnwApi"
      # childStart LlnwApi.startLink unit

    agentSup =
      buildChild
      # childType Supervisor
      # childId "agentSup"
      # childStart AgentSup.startLink args

    webServer =
      buildChild
      # childType Worker
      # childId "web"
      # childStart Web.startLink args.canaryState
