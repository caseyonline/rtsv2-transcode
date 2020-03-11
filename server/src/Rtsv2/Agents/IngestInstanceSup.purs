 module Rtsv2.Agents.IngestInstanceSup
  ( startLink
  , startIngest
  ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Process.Raw (Pid)
import Pinto (SupervisorName, ok)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Names as Names
import Shared.LlnwApiTypes (StreamDetails, StreamPublish)
import Shared.Stream (IngestKey)

serverName :: SupervisorName
serverName = Names.ingestInstanceSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startIngest :: IngestKey -> StreamPublish -> StreamDetails -> String -> Int -> Pid -> Effect Unit
startIngest ingestKey streamPublish streamDetails remoteAddress remotePort handlerPid = do
  let
    startArgs = { streamPublish
                , streamDetails
                , ingestKey
                , remoteAddress
                , remotePort
                , handlerPid
                }
  ok =<< Sup.startSimpleChild childTemplate serverName { childStartLink: IngestInstance.startLink startArgs
                                                       , childStopAction: IngestInstance.stopAction ingestKey
                                                       , serverName: Names.ingestInstanceStateName ingestKey
                                                       , domain: IngestInstance.domain
                                                       }

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

childTemplate :: Pinto.ChildTemplate (CachedInstanceState.StartArgs IngestInstance.CachedState)
childTemplate = Pinto.ChildTemplate (CachedInstanceState.startLink)
