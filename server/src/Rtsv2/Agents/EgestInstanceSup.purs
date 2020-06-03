module Rtsv2.Agents.EgestInstanceSup
       ( startLink
       , findEgest
       , startLocalEgest
       , startLocalOrRemoteEgest
       , isAgentAvailable
       )
       where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, note)
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, nil, singleton, (:))
import Erl.Process.Raw (Pid)
import Logger as Logger
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Pinto.Types (startOkAS)
import Prim.Row as Row
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.StreamRelaySup as StreamRelayInstance
import Rtsv2.Config (LoadConfig)
import Rtsv2.Load as Load
import Rtsv2.LoadTypes (LoadCheckResult)
import Rtsv2.LoadTypes as LoadTypes
import Rtsv2.Names as Names
import Rtsv2.NodeManager as NodeManager
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Types (LocalOrRemote(..), LocalResourceResp, LocationResp, ResourceFailed(..), ResourceResp)
import Rtsv2.Utils (noprocToMaybe)
import Shared.Rtsv2.Agent (Agent(..))
import Shared.Rtsv2.Router.Endpoint.System as System
import Shared.Rtsv2.Stream (AggregatorKey(..), EgestKey(..), SlotId, SlotRole)
import Shared.Rtsv2.Types (CanaryState, FailureReason(..), OnBehalfOf, Server, ServerLoad, extractPoP, serverLoadToServer)
import SpudGun as SpudGun

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
isAgentAvailable :: Effect Boolean
isAgentAvailable = Pinto.isRegistered serverName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

findEgest :: LoadConfig -> CanaryState -> SlotId -> SlotRole -> Effect LocationResp
findEgest loadConfig canary slotId slotRole = do
  thisServer <- PoPDefinition.getThisServer
  mIngestAggregator <- IntraPoP.whereIsIngestAggregatorWithPayload (AggregatorKey slotId slotRole)
  case mIngestAggregator of
    Nothing ->  pure $ Left NotFound
    Just {payload: slotCharacteristics, server: aggregator} ->
      let
        payload = {slotId, slotRole, aggregatorPoP: extractPoP aggregator, slotCharacteristics}
      in
       findEgest' loadConfig canary thisServer egestKey payload
  where
    egestKey = (EgestKey slotId slotRole)

startLocalEgest :: LoadConfig -> OnBehalfOf -> CreateEgestPayload -> Effect (LocalResourceResp Server)
startLocalEgest loadConfig onBehalfOf payload@{slotCharacteristics} =
  NodeManager.launchLocalAgent Egest onBehalfOf (Load.hasCapacityForEgestInstance slotCharacteristics loadConfig) launchLocal
  where
    launchLocal _ = startEgest payload

startLocalOrRemoteEgest :: LoadConfig -> OnBehalfOf -> CreateEgestPayload -> Effect (ResourceResp Server)
startLocalOrRemoteEgest loadConfig onBehalfOf payload@{slotCharacteristics} =
 NodeManager.launchLocalOrRemoteAgent Egest onBehalfOf (Load.hasCapacityForEgestInstance slotCharacteristics loadConfig) launchLocal launchRemote
 where
   launchLocal _ = startEgest payload
   launchRemote remote = do
     url <- System.makeUrl remote System.EgestE
     either (const false) (const true) <$> SpudGun.postJson url payload

------------------------------------------------------------------------------
-- Supervisor callbacks
------------------------------------------------------------------------------
init :: Effect Sup.SupervisorSpec
init = do
  logStart "Egest Supervisor starting" {}
  pure
    $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.SimpleOneForOne
    # Sup.supervisorChildren
    ( ( buildChild
        # childType Worker
        # childId "egestAgent"
        # childStartTemplate childTemplate
        # childRestart Transient
      )
      : nil
    )

childTemplate :: Pinto.ChildTemplate (CachedInstanceState.StartArgs EgestInstance.CachedState)
childTemplate = Pinto.ChildTemplate (CachedInstanceState.startLink)

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------
serverName :: SupervisorName
serverName = Names.egestInstanceSupName

findEgest' :: LoadConfig -> CanaryState -> Server -> EgestKey -> CreateEgestPayload -> Effect LocationResp
findEgest' loadConfig canary thisServer egestKey payload@{slotCharacteristics} = runExceptT
  $ ExceptT getLocal
  <|> ExceptT getRemote
  <|> ExceptT createResourceAndRecurse
  where
    getLocal = do
      local <- noprocToMaybe $ EgestInstance.pendingClient egestKey
      pure $ note NotFound $ (const (Local thisServer)) <$> local

    getRemote = do
      egests <- IntraPoP.whereIsEgestWithLoad egestKey
      let
        candidates = filterMap capacityForClient egests
      eEgest <- IntraPoP.selectCandidate candidates
      pure $ lmap (\_ -> NotFound) eEgest

    createResourceAndRecurse :: Effect LocationResp
    createResourceAndRecurse = do
      eLaunchResp <- NodeManager.launchLocalOrRemoteAgent Egest canary (Load.hasCapacityForEgestInstance slotCharacteristics loadConfig) launchLocal launchRemote
      case eLaunchResp of
        Left error -> pure $ Left NoResource
        Right _ -> do
          findEgest' loadConfig canary thisServer egestKey payload
      where
        launchLocal _ = startEgest payload
        launchRemote remote = do
          url <- System.makeUrl remote System.EgestE
          -- todo - if remote then need to sleep before recurse to allow intra-pop disemination
          either (const false) (const true) <$> SpudGun.postJson url payload

    capacityForClient :: ServerLoad -> Maybe (Tuple Server LoadCheckResult)
    capacityForClient serverLoad =
      let
        loadCheckResult = Load.hasCapacityForEgestClient slotCharacteristics loadConfig serverLoad
      in
       if LoadTypes.canLaunch loadCheckResult then
         Just (Tuple (serverLoadToServer serverLoad) loadCheckResult)
       else
         Nothing

startEgest :: CreateEgestPayload -> Effect (Either ResourceFailed Pid)
startEgest payload@{slotId, slotRole} =
  let
    egestKey = EgestKey slotId slotRole
    parentCallbacks =
      { startLocalOrRemoteStreamRelay: StreamRelayInstance.startLocalOrRemoteStreamRelay
      , startLocalOrRemoteEgest
      }
  in
    (note LaunchFailed <<< startOkAS) <$>
    Sup.startSimpleChild childTemplate serverName { childStartLink: EgestInstance.startLink parentCallbacks payload
                                                  , childStopAction: EgestInstance.stopAction egestKey
                                                  , serverName: Names.egestInstanceStateName egestKey
                                                  , domain: EgestInstance.domain
                                                  }

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = serverName # Names.toDomain # singleton

logInfo :: forall report. Row.Lacks "text" report => String -> { | report } -> Effect Unit
logInfo = Logger.doLog domain Logger.info

logWarning :: forall report. Row.Lacks "text" report => String -> { | report } -> Effect Unit
logWarning = Logger.doLog domain Logger.warning

logStart :: forall report. Row.Lacks "text" report => String -> { | report } -> Effect Unit
logStart = Logger.doLogEvent domain Logger.Start Logger.info
