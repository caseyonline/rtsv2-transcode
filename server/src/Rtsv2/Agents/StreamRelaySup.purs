module Rtsv2.Agents.StreamRelaySup
       ( isAvailable
       , startLink
       , startRelay
       )
       where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto (SupervisorName, isRegistered)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.PersistentInstanceState as PersistentInstanceState
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelayInstanceSup as StreamRelayInstanceSup
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload)
import Rtsv2.Names as Names

isAvailable :: Effect Boolean
isAvailable = isRegistered serverName

serverName :: SupervisorName
serverName = Names.streamRelaySupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

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

childTemplate :: Pinto.ChildTemplate (PersistentInstanceState.StartArgs StreamRelayInstance.PersistentState)
childTemplate = Pinto.ChildTemplate (PersistentInstanceState.startLink)
