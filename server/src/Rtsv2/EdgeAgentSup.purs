module Rtsv2.EdgeAgentSup where

import Effect (Effect)
import Erl.Data.List (nil)
import Prelude
import Pinto as Pinto
import Pinto.Sup as Sup

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink "edgeAgentSup" init

init :: Effect Sup.SupervisorSpec
init = do
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.OneForOne
    # Sup.supervisorChildren nil
