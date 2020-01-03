 module Rtsv2.Agents.IngestInstanceSup
  ( isAvailable
  , startLink
  , startIngest
  ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Utils as ErlUtils
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Names as Names
import Rtsv2.Agents.IngestInstance as IngestInstance
import Shared.Stream (StreamVariantId)

isAvailable :: Effect Boolean
isAvailable = ErlUtils.isRegistered serverName

serverName :: String
serverName = Names.ingestAgentInstanceSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startIngest :: StreamVariantId -> Effect Unit
startIngest streamVariantId = do
  result <- Sup.startSimpleChild childTemplate serverName streamVariantId
  case result of
    Pinto.AlreadyStarted pid -> pure unit
    Pinto.Started pid -> pure unit

init :: Effect Sup.SupervisorSpec
init = do
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.SimpleOneForOne
    # Sup.supervisorChildren
        ( ( buildChild
              # childType Worker
              # childId "ingestAgent"
              # childStartTemplate childTemplate
              # childRestart Transient
          )
            : nil
        )

childTemplate :: Pinto.ChildTemplate StreamVariantId
childTemplate = Pinto.ChildTemplate (IngestInstance.startLink)
