module Rtsv2EdgeAgentSup where

import Effect (Effect)
import Erl.Data.List (nil)
import Prelude

import Shared.Agents as Agents
import Pinto as Pinto
import Pinto.Sup (startLink) as Sup
import Pinto.Sup
import Gproc as Gproc

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink "edgeAgentSup" init

init :: Effect SupervisorSpec
init = do
  _ <- Gproc.register Agents.EdgeAgent
  pure $ buildSupervisor
                # supervisorStrategy OneForOne
                # supervisorChildren nil
