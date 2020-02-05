module Rtsv2.Agents.StreamRelay.InstanceSup
       ( startLink
       , startRelay
       , isAvailable
       )
       where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Rtsv2.Names as Names
import Logger (Logger)
import Logger as Logger
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.StreamRelay.Types (CreateRelayPayload)
import Rtsv2.Agents.StreamRelay.Instance as StreamRelayInstance
import Shared.Agent as Agent

serverName :: SupervisorName
serverName = Names.streamRelayInstanceSupName

isAvailable :: Effect Boolean
isAvailable = Pinto.isRegistered serverName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startRelay :: CreateRelayPayload -> Effect Pinto.StartLinkResult
startRelay createPayload = do
  Sup.startSimpleChild childTemplate serverName createPayload

init :: Effect Sup.SupervisorSpec
init = do
  logInfo "StreamRelay Supervisor starting" {}
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.SimpleOneForOne
    # Sup.supervisorChildren
        ( ( buildChild
              # childType Worker
              # childId "streamRelayAgent"
              # childStartTemplate childTemplate
              # childRestart Transient
          )
            : nil
        )

childTemplate :: Pinto.ChildTemplate CreateRelayPayload
childTemplate = Pinto.ChildTemplate (StreamRelayInstance.startLink)

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.StreamRelay :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains