module Rtsv2.Agents.IngestAggregatorSup where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..))
import Pinto.Sup as Sup
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceState as IngestAggregatorInstanceState
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Names as Names

startLink :: Unit -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink Names.ingestAggregatorSupName init

init :: Effect SupervisorSpec
init = do
  pure $ Sup.buildSupervisor
    # Sup.supervisorIntensity 50
    # Sup.supervisorStrategy OneForAll
    # Sup.supervisorChildren childSpecs

  where
    childSpecs =
      ( Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "ingestAggregatorInstanceState"
        # Sup.childStart IngestAggregatorInstanceState.startLink IngestAggregatorInstance.stateServerName
      )
      : ( Sup.buildChild
          # Sup.childType Supervisor
          # Sup.childId "ingestAggregatorInstanceSup"
          # Sup.childStart IngestAggregatorInstanceSup.startLink unit
        )
      : nil
