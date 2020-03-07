module Rtsv2.Agents.IngestAggregatorInstanceSup
       ( startLink
       )
       where


import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Logger (spy)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..))
import Pinto.Sup as Sup
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Names as Names
import Shared.LlnwApiTypes (StreamDetails)

startLink :: StreamDetails -> IngestAggregatorInstance.StateServerName -> Effect Pinto.StartLinkResult
startLink streamDetails stateServerName = Sup.startLink (spy "XXX IngestAggregatorInstanceSup" (Names.ingestAggregatorInstanceSupName (IngestAggregatorInstance.streamDetailsToAggregatorKey streamDetails))) (init streamDetails stateServerName)

init :: StreamDetails -> IngestAggregatorInstance.StateServerName -> Effect SupervisorSpec
init streamDetails stateServerName = do
  pure $ Sup.buildSupervisor
    # Sup.supervisorIntensity 50
    # Sup.supervisorStrategy OneForAll
    # Sup.supervisorChildren childSpecs

  where
    childSpecs =
      ( Sup.buildChild
        # Sup.childType Worker
        # Sup.childId "ingestAggregatorInstance"
        # Sup.childStart (IngestAggregatorInstance.startLink streamDetails) stateServerName
      )
      : nil
