module Rtsv2.Agents.IngestInstanceSup
  ( startLink
  , startLocalRtmpIngest
  , startLocalWebRTCIngest
  ) where

import Prelude

import Data.Either (note)
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Process.Raw (Pid)
import Pinto (StartChildResult, SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Pinto.Types (startOk)
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Config (LoadConfig)
import Rtsv2.Load as Load
import Rtsv2.LoadTypes (LoadCheckResult)
import Rtsv2.Names as Names
import Rtsv2.NodeManager as NodeManager
import Shared.Rtsv2.Agent (Agent(..), SlotCharacteristics)
import Shared.Rtsv2.LlnwApiTypes (StreamDetails, StreamPublish, slotDetailsToSlotCharacteristics)
import Shared.Rtsv2.Stream (IngestKey)
import Shared.Rtsv2.Types (Canary, ResourceResp, Server, ServerLoad)

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startLocalRtmpIngest :: LoadConfig -> Canary -> IngestKey -> StreamPublish -> StreamDetails -> String -> Int -> Pid -> Effect (ResourceResp Server)
startLocalRtmpIngest = startLocalIngest Load.hasCapacityForRtmpIngest

startLocalWebRTCIngest :: LoadConfig -> Canary -> IngestKey -> StreamPublish -> StreamDetails -> String -> Int -> Pid -> Effect (ResourceResp Server)
startLocalWebRTCIngest = startLocalIngest Load.hasCapacityForWebRTCIngest

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
              # childId "ingestAgent"
              # childStartTemplate childTemplate
              # childRestart Transient
          )
          : nil
        )

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------
startLocalIngest :: (SlotCharacteristics -> LoadConfig -> ServerLoad -> LoadCheckResult) -> LoadConfig -> Canary -> IngestKey -> StreamPublish -> StreamDetails -> String -> Int -> Pid -> Effect (ResourceResp Server)
startLocalIngest capacityFun loadConfig canary ingestKey streamPublish streamDetails@{slot} remoteAddress remotePort handlerPid =
  let
    slotCharacteristics = slotDetailsToSlotCharacteristics slot
  in
    NodeManager.launchLocalAgent Ingest canary (capacityFun slotCharacteristics loadConfig) launchLocal
  where
    launchLocal _ =
      (note unit <<< startOk) <$> startIngest ingestKey streamPublish streamDetails canary remoteAddress remotePort handlerPid

startIngest :: IngestKey -> StreamPublish -> StreamDetails -> Canary -> String -> Int -> Pid -> Effect StartChildResult
startIngest ingestKey streamPublish streamDetails canary remoteAddress remotePort handlerPid = do
  let
    startArgs = { streamPublish
                , streamDetails
                , canary
                , ingestKey
                , remoteAddress
                , remotePort
                , handlerPid
                }
  Sup.startSimpleChild childTemplate serverName { childStartLink: IngestInstance.startLink startArgs
                                                , childStopAction: IngestInstance.stopAction ingestKey
                                                , serverName: Names.ingestInstanceStateName ingestKey
                                                , domain: IngestInstance.domain
                                                  }

serverName :: SupervisorName
serverName = Names.ingestInstanceSupName

childTemplate :: Pinto.ChildTemplate (CachedInstanceState.StartArgs IngestInstance.CachedState)
childTemplate = Pinto.ChildTemplate (CachedInstanceState.startLink)
