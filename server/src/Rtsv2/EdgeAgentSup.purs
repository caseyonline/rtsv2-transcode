module Rtsv2.EdgeAgentSup where

import Effect (Effect)
import Erl.Data.List (nil)
import Prelude
import Shared.Agent as Agent
import Pinto as Pinto
import Pinto.Sup as Sup
import Gproc as Gproc

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink "edgeAgentSup" init

init :: Effect Sup.SupervisorSpec
init = do
  _ <- Gproc.register Agent.EdgeAgent
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.OneForOne
    # Sup.supervisorChildren nil
