module Rtsv2.Agents.IngestAggregatorSup
       ( isAvailable
       , startLink
       , startAggregator
       )
       where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto (StartChildResult, SupervisorName, isRegistered)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Names as Names
import Shared.LlnwApiTypes (StreamDetails)

isAvailable :: Effect Boolean
isAvailable = isRegistered serverName

serverName :: SupervisorName
serverName = Names.ingestAggregatorSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startAggregator :: StreamDetails -> Effect StartChildResult
startAggregator streamDetails =
  let
    aggregatorKey = IngestAggregatorInstance.streamDetailsToAggregatorKey streamDetails
  in
   Sup.startSimpleChild childTemplate serverName { childStartLink: IngestAggregatorInstanceSup.startLink aggregatorKey streamDetails
                                                 , childStopAction: IngestAggregatorInstance.stopAction aggregatorKey
                                                 , serverName: Names.ingestAggregatorInstanceStateName aggregatorKey
                                                 , domain: IngestAggregatorInstance.domain
                                                 }

init :: Effect Sup.SupervisorSpec
init = do
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.SimpleOneForOne
    # Sup.supervisorChildren
        ( ( buildChild
              # childType Worker
              # childId "ingestAggregatorAgentState"
              # childStartTemplate childTemplate
              # childRestart Transient
          )
          : nil
        )

childTemplate :: Pinto.ChildTemplate (CachedInstanceState.StartArgs IngestAggregatorInstance.CachedState)
childTemplate = Pinto.ChildTemplate (CachedInstanceState.startLink)
