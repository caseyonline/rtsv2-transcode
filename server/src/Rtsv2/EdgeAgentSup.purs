module Rtsv2.EdgeAgentSup where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil)
import Erl.Utils as ErlUtils
import Pinto as Pinto
import Pinto.Sup as Sup
import Shared.Agent as Agent

serverName :: String
serverName = show Agent.Edge

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

isAvailable :: Effect Boolean
isAvailable = ErlUtils.isRegistered serverName

init :: Effect Sup.SupervisorSpec
init = do
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.OneForOne
    # Sup.supervisorChildren nil
