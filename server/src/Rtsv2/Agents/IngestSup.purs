module Rtsv2.Agents.IngestSup
  ( isAgentAvailable
  , startLink
  ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), buildChild, childId, childStart, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Agents.IngestOneForOneSup as IngestOneForOneSup
import Rtsv2.Agents.IngestRtmpServer as IngestRtmpServer
import Rtsv2.Names as Names

isAgentAvailable :: Effect Boolean
isAgentAvailable = Pinto.isRegistered serverName

serverName :: SupervisorName
serverName = Names.ingestSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

init :: Effect Sup.SupervisorSpec
init = do
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.OneForAll
    # Sup.supervisorChildren
        ( ( buildChild
              # childType Worker
              # childId "ingestRtmpServer"
              # childStart IngestRtmpServer.startLink unit
            )
          : ( buildChild
              # childType Supervisor
              # childId "ingestOneForOne"
              # childStart IngestOneForOneSup.startLink unit
          )
          : ( buildChild
              # childType Supervisor
              # childId "ingestAgentInstance"
              # childStart IngestInstanceSup.startLink unit
          )
            : nil
        )
