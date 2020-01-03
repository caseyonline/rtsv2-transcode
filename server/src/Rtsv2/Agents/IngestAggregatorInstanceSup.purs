module Rtsv2.Agents.IngestAggregatorInstanceSup
       ( isAvailable
       , startLink
       , startAggregator
       )
       where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Names as Names
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Shared.Stream (StreamId)

isAvailable :: Effect Boolean
isAvailable = Names.ingestAggregatorAgentSupRegistered

serverName :: String
serverName = Names.ingestAggregatorAgentSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startAggregator :: StreamId -> Effect Unit
startAggregator streamId = do
  result <- Sup.startSimpleChild childTemplate serverName streamId
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

childTemplate :: Pinto.ChildTemplate StreamId
childTemplate = Pinto.ChildTemplate (IngestAggregatorInstance.startLink)
