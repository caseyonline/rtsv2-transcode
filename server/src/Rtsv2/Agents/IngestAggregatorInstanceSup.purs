module Rtsv2.Agents.IngestAggregatorInstanceSup
       ( isAvailable
       , startLink
       , startAggregator
       )
       where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Names as Names
import Shared.LlnwApiTypes (StreamDetails)


isAvailable :: Effect Boolean
isAvailable = Names.isRegistered serverName

serverName :: SupervisorName
serverName = Names.ingestAggregatorInstanceSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startAggregator :: StreamDetails -> Effect Unit
startAggregator streamDetails = do
  result <- Sup.startSimpleChild childTemplate serverName streamDetails
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
              # childId "ingestAggregatorAgent"
              # childStartTemplate childTemplate
              # childRestart Transient
          )
          : nil
        )

childTemplate :: Pinto.ChildTemplate StreamDetails
childTemplate = Pinto.ChildTemplate (IngestAggregatorInstance.startLink)
