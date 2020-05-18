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
import Shared.Rtsv2.Types (Canary)

startLink :: Canary -> Effect Pinto.StartLinkResult
startLink args = Sup.startLink (Local (atom "rtsv2ActiveSup")) (init args)

init :: Canary -> Effect SupervisorSpec
init canary = do
  pure $ buildSupervisor
    # supervisorStrategy OneForOne
    # supervisorChildren
    ( agentSup
      : dataObject
      : nil
    )
  where
    dataObject =
      buildChild
      # childType Worker
      # childId "dataObject"
      # childStart DataObject.startLink unit

    agentSup =
      buildChild
      # childType Supervisor
      # childId "agentSup"
      # childStart AgentSup.startLink canary
