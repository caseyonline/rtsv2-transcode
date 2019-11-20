module Rtsv2EdgeAgentSup where

import Effect
import Erl.Data.List
import Prelude

import Pinto as Pinto
import Pinto.Sup (startLink, startChild) as Sup
import Pinto.Sup
import Rtsv2Web as Rtsv2Web
import Rtsv2Config as Rtsv2Config

startLink :: Unit -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink "edgeAgentSup" init

init :: Effect SupervisorSpec
init = do
  pure $ buildSupervisor
                # supervisorStrategy SimpleOneForOne
                # supervisorChildren nil
