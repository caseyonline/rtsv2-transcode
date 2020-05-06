module Rtsv2.Agents.IngestAggregatorSup
       ( isAgentAvailable
       , startLink
       , startLocalAggregator
       , startLocalOrRemoteAggregator
       )
       where

import Prelude

import Data.Either (either)
import Data.Maybe (maybe)
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto (StartChildResult, SupervisorName, isRegistered)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Pinto.Types (startOkAS)
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.IngestAggregatorInstance (CreateAggregatorPayload)
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Config (LoadConfig)
import Rtsv2.Load as Load
import Rtsv2.Names as Names
import Shared.Rtsv2.LlnwApiTypes (slotDetailsToSlotCharacteristics)
import Shared.Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Rtsv2.Types (Server, ResourceResp)
import SpudGun as SpudGun

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
isAgentAvailable :: Effect Boolean
isAgentAvailable = isRegistered serverName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startLocalAggregator :: LoadConfig -> CreateAggregatorPayload -> Effect (ResourceResp Server)
startLocalAggregator loadConfig payload@{streamDetails} =
  let
    slotCharacteristics = slotDetailsToSlotCharacteristics streamDetails.slot
  in
    Load.launchLocalGeneric (Load.hasCapacityForAggregator slotCharacteristics loadConfig) launchLocal
  where
    launchLocal _ = do
      (maybe false (const true) <<< startOkAS) <$> startAggregator payload

startLocalOrRemoteAggregator :: LoadConfig -> CreateAggregatorPayload -> Effect (ResourceResp Server)
startLocalOrRemoteAggregator loadConfig payload@{streamDetails} =
  let
    slotCharacteristics = slotDetailsToSlotCharacteristics streamDetails.slot
  in
    Load.launchLocalOrRemoteGeneric (Load.hasCapacityForAggregator slotCharacteristics loadConfig) launchLocal launchRemote
  where
    launchLocal _ = do
      (maybe false (const true) <<< startOkAS) <$> startAggregator payload
    launchRemote idleServer =
      either (const false) (const true) <$> SpudGun.postJson (makeUrl idleServer IngestAggregatorsE) payload

------------------------------------------------------------------------------
-- Supervisor callbacks
------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------
serverName :: SupervisorName
serverName = Names.ingestAggregatorSupName

startAggregator :: CreateAggregatorPayload -> Effect StartChildResult
startAggregator payload@{streamDetails} =
  let
    aggregatorKey = IngestAggregatorInstance.streamDetailsToAggregatorKey streamDetails
  in
   Sup.startSimpleChild childTemplate serverName { childStartLink: IngestAggregatorInstanceSup.startLink aggregatorKey payload
                                                 , childStopAction: IngestAggregatorInstance.stopAction aggregatorKey
                                                 , serverName: Names.ingestAggregatorInstanceStateName aggregatorKey
                                                 , domain: IngestAggregatorInstance.domain
                                                 }

childTemplate :: Pinto.ChildTemplate (CachedInstanceState.StartArgs IngestAggregatorInstance.CachedState)
childTemplate = Pinto.ChildTemplate (CachedInstanceState.startLink)
