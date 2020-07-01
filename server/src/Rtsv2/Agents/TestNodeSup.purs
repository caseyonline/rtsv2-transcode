module Rtsv2.Agents.TestNodeSup
       ( isAgentAvailable
       , startLink
       )
       where


import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStart, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.TestNodeServer as TestNodeServer
import Rtsv2.Names as Names
import Effect.Console (log, logShow)

isAgentAvailable :: Effect Boolean
isAgentAvailable = Pinto.isRegistered serverName

serverName :: SupervisorName
serverName = Names.testNodeSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

init :: Effect Sup.SupervisorSpec
init = do
  log "------------------------------- TestNodeSup --------------------------------"
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.OneForAll
    # Sup.supervisorChildren
        ( ( buildChild
              # childType Worker
              # childId "TestNodeServer"
              # childStart TestNodeServer.startLink unit
              # childRestart Transient
            )
            : nil
        )
