 module Rtsv2.IngestAgentSup
  ( startLink
  , startIngest
  ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.IngestAgent as Ingest
import Shared.Stream (StreamVariantId)

serverName :: String
serverName = "ingestAgentSup"

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
childTemplate = Pinto.ChildTemplate (Ingest.startLink)
