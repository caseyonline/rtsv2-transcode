module Rtsv2.Agents.StreamRelaySup
       ( isAgentAvailable
       , startLink
       , findOrStart
       , startLocalStreamRelay
       , startLocalOrRemoteStreamRelay
       )
       where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto (SupervisorName, isRegistered)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Pinto.Types (startOkAS)
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelayInstanceSup as StreamRelayInstanceSup
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload)
import Rtsv2.Config (LoadConfig)
import Rtsv2.Load as Load
import Rtsv2.Names as Names
import Shared.Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Rtsv2.Stream (RelayKey(..))
import Shared.Rtsv2.Types (Server, LocalOrRemote(..), ResourceResp)
import SpudGun as SpudGun

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
isAgentAvailable :: Effect Boolean
isAgentAvailable = isRegistered serverName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

-- If there's an existing node running the agent instance, return it
-- If there isn't, then find an idle server
-- If that server is local, then start the agent instance
-- If that server is remote, then return the remote server to allow the caller to redirect
findOrStart :: LoadConfig -> CreateRelayPayload -> Effect (ResourceResp Server)
findOrStart loadConfig payload@{slotCharacteristics} = do
  mExisting <- IntraPoP.whereIsStreamRelayWithLocalOrRemote relayKey
  case mExisting of
    Just existing ->
      pure $ Right existing
    Nothing -> do
      newResourceResp <- IntraPoP.getIdleServer $ Load.hasCapacityForStreamRelay slotCharacteristics loadConfig
      case newResourceResp of
        Left err -> do
          pure $ Left err

        Right (Local server) -> do
          _ <- startRelay payload
          pure $ Right $ Local server

        Right (Remote server) -> do
          pure $ Right $ Remote server
  where
    relayKey = RelayKey payload.slotId payload.slotRole

startLocalStreamRelay :: LoadConfig -> CreateRelayPayload -> Effect (ResourceResp Server)
startLocalStreamRelay loadConfig payload@{slotCharacteristics} =
  Load.launchLocalGeneric (Load.hasCapacityForStreamRelay slotCharacteristics loadConfig) launchLocal
  where
    launchLocal _ =
      (maybe false (const true) <<< startOkAS) <$> startRelay payload

startLocalOrRemoteStreamRelay :: LoadConfig -> CreateRelayPayload -> Effect (ResourceResp Server)
startLocalOrRemoteStreamRelay loadConfig payload@{slotCharacteristics} =
  Load.launchLocalOrRemoteGeneric (Load.hasCapacityForStreamRelay slotCharacteristics loadConfig) launchLocal launchRemote
  where
    launchLocal _ =
      (maybe false (const true) <<< startOkAS) <$> startRelay payload
    launchRemote idleServer =
      either (const false) (const true) <$> SpudGun.postJson ( makeUrl idleServer RelayE) payload

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
serverName = Names.streamRelaySupName

startRelay :: CreateRelayPayload -> Effect Pinto.StartChildResult
startRelay createPayload =
  let
    relayKey = StreamRelayInstance.payloadToRelayKey createPayload
  in
   Sup.startSimpleChild childTemplate serverName { childStartLink: StreamRelayInstanceSup.startLink relayKey createPayload
                                                 , childStopAction: StreamRelayInstance.stopAction relayKey
                                                 , serverName: Names.streamRelayInstanceStateName relayKey
                                                 , domain: StreamRelayInstance.domain
                                                 }


childTemplate :: Pinto.ChildTemplate (CachedInstanceState.StartArgs StreamRelayInstance.CachedState)
childTemplate = Pinto.ChildTemplate (CachedInstanceState.startLink)
