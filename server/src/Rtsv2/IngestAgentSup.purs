 module Rtsv2.IngestAgentSup
  ( isAvailable
  , startLink
  ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Utils as ErlUtils
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStart, childType)
import Pinto.Sup as Sup
import Rtsv2.IngestAgent as Ingest
import Rtsv2.IngestAgentInstanceSup as IngestAgentInstanceSup
import Rtsv2.IngestRtmpServer as IngestRtmpServer
import Shared.Stream (StreamVariantId)

isAvailable :: Effect Boolean
isAvailable = ErlUtils.isRegistered serverName

serverName :: String
serverName = "ingestAgentSup"

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
              # childStart IngestAgentInstanceSup.startLink unit
              # childRestart Transient
          )
            : nil
        )

childTemplate :: Pinto.ChildTemplate StreamVariantId
childTemplate = Pinto.ChildTemplate (Ingest.startLink)
