module Rtsv2.IngestAgentSup where

import Effect (Effect)
import Erl.Data.List (nil)
import Prelude
import Shared.Agents as Agents
import Pinto as Pinto
import Pinto.Sup as Sup
import Gproc as Gproc

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink "ingestAgentSup" init

init :: Effect Sup.SupervisorSpec
init = do
  _ <- Gproc.register Agents.IngestAgent
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.OneForOne
    # Sup.supervisorChildren nil
