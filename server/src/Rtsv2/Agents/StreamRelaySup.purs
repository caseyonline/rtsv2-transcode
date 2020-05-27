module Rtsv2.Agents.StreamRelaySup
       ( isAgentAvailable
       , startLink
       , findOrStart
       , startLocalStreamRelay
       , startLocalOrRemoteStreamRelay
       )
       where

import Prelude

import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Process.Raw (Pid)
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
import Rtsv2.NodeManager as NodeManager
import Rtsv2.Types (ResourceFailed(..), ResourceResp, LocalResourceResp)
import Shared.Rtsv2.Agent (Agent(..))
import Shared.Rtsv2.Router.Endpoint.System as System
import Shared.Rtsv2.Stream (RelayKey(..))
import Shared.Rtsv2.Types (OnBehalfOf, Server)
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
findOrStart :: LoadConfig -> OnBehalfOf -> CreateRelayPayload -> Effect (ResourceResp Server)
findOrStart loadConfig onBehalfOf payload@{slotCharacteristics} = do
  mExisting <- IntraPoP.whereIsStreamRelayWithLocalOrRemote relayKey
  case mExisting of
    Just existing ->
      pure $ Right existing
    Nothing ->
      startLocalOrRemoteStreamRelay loadConfig onBehalfOf payload
  where
    relayKey = RelayKey payload.slotId payload.slotRole

startLocalStreamRelay :: LoadConfig -> OnBehalfOf -> CreateRelayPayload -> Effect (LocalResourceResp Server)
startLocalStreamRelay loadConfig onBehalfOf payload@{slotCharacteristics} =
  NodeManager.launchLocalAgent StreamRelay onBehalfOf (Load.hasCapacityForStreamRelay slotCharacteristics loadConfig) launchLocal
  where
    launchLocal _ = startRelay payload

startLocalOrRemoteStreamRelay :: LoadConfig -> OnBehalfOf -> CreateRelayPayload -> Effect (ResourceResp Server)
startLocalOrRemoteStreamRelay loadConfig onBehalfOf payload@{slotCharacteristics} =
  NodeManager.launchLocalOrRemoteAgent StreamRelay onBehalfOf (Load.hasCapacityForStreamRelay slotCharacteristics loadConfig) launchLocal launchRemote
  where
    launchLocal _ = startRelay payload
    launchRemote idleServer = do
      url <- System.makeUrl idleServer System.RelayE
      either (const false) (const true) <$> SpudGun.postJson url payload

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

startRelay :: CreateRelayPayload -> Effect (Either ResourceFailed Pid)
startRelay createPayload =
  let
    relayKey = StreamRelayInstance.payloadToRelayKey createPayload
  in
  (note LaunchFailed <<< startOkAS) <$>
  Sup.startSimpleChild childTemplate serverName { childStartLink: StreamRelayInstanceSup.startLink relayKey createPayload
                                                 , childStopAction: StreamRelayInstance.stopAction relayKey
                                                 , serverName: Names.streamRelayInstanceStateName relayKey
                                                 , domain: StreamRelayInstance.domain
                                                 }


childTemplate :: Pinto.ChildTemplate (CachedInstanceState.StartArgs StreamRelayInstance.CachedState)
childTemplate = Pinto.ChildTemplate (CachedInstanceState.startLink)
