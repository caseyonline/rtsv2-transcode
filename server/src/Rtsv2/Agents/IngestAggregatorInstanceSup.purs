module Rtsv2.Agents.IngestAggregatorInstanceSup
       ( startLink
       )
       where


import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..))
import Pinto.Sup as Sup
import Rtsv2.Agents.IngestAggregatorInstance (CreateAggregatorPayload, ParentCallbacks)
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Names as Names
import Shared.Rtsv2.Stream (AggregatorKey)

startLink :: AggregatorKey -> ParentCallbacks -> CreateAggregatorPayload -> IngestAggregatorInstance.StateServerName -> Effect Pinto.StartLinkResult
startLink aggregatorKey parentCallbacks payload stateServerName = Sup.startLink (Names.ingestAggregatorInstanceSupName aggregatorKey) (init aggregatorKey parentCallbacks payload stateServerName)

init :: AggregatorKey -> ParentCallbacks -> CreateAggregatorPayload -> IngestAggregatorInstance.StateServerName -> Effect SupervisorSpec
init aggregatorKey parentCallbacks payload stateServerName = do
  pure $ Sup.buildSupervisor
    # Sup.supervisorIntensity 50
    # Sup.supervisorStrategy OneForAll
    # Sup.supervisorChildren childSpecs

  where
    childSpecs =
      ( Sup.buildChild
        # Sup.childType Worker
        # Sup.childId "ingestAggregatorInstance"
        # Sup.childStart (IngestAggregatorInstance.startLink aggregatorKey parentCallbacks payload) stateServerName
      )
      : nil
