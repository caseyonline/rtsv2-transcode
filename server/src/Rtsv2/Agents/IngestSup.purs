 module Rtsv2.Agents.IngestSup
  ( isAvailable
  , startLink
  ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStart, childType)
import Pinto.Sup as Sup
import Rtsv2.Names as Names
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Agents.IngestRtmpServer as IngestRtmpServer
import Shared.Stream (StreamVariantId)

isAvailable :: Effect Boolean
isAvailable = Names.ingestAgentSupRegistered

serverName :: String
serverName = Names.ingestAgentSupName

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
              # childRestart Transient
          )
          : ( buildChild
              # childType Supervisor
              # childId "ingestAgentInstance"
              # childStart IngestInstanceSup.startLink unit
              # childRestart Transient
          )
            : nil
        )

childTemplate :: Pinto.ChildTemplate StreamVariantId
childTemplate = Pinto.ChildTemplate (IngestInstance.startLink)
