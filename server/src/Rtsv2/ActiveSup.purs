module Rtsv2.ActiveSup where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Pinto (ServerName(..))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..), buildChild, buildSupervisor, childId, childRestart, childStart, childType, supervisorChildren, supervisorStrategy)
import Pinto.Sup as Sup
import Rtsv2.Agents.AgentSup as AgentSup
import Rtsv2.DataObject as DataObject
import Rtsv2.LlnwApi as LlnwApi
import Rtsv2.Types (AgentSupStartArgs)

startLink :: AgentSupStartArgs -> Effect Pinto.StartLinkResult
startLink args = Sup.startLink (Local (atom "rtsv2ActiveSup")) (init args)

init :: AgentSupStartArgs -> Effect SupervisorSpec
init args = do
  pure $ buildSupervisor
    # supervisorStrategy OneForOne
    # supervisorChildren
    ( agentSup
      : dataObject
      : llnwApi
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
