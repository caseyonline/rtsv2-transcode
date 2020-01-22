module Rtsv2.Agents.StreamRelayInstanceSup
       ( startLink
       , startRelay
       , isAvailable
       )
       where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Logger (Logger)
import Logger as Logger
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.StreamRelayInstance (CreateRelayPayload)
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Names as Names
import Shared.Agent as Agent

serverName :: SupervisorName
serverName = Names.streamRelayInstanceSupName

isAvailable :: Effect Boolean
isAvailable = Names.isRegistered serverName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startRelay :: CreateRelayPayload -> Effect Pinto.StartChildResult
startRelay createPayload = do
  Sup.startSimpleChild childTemplate serverName createPayload

init :: Effect Sup.SupervisorSpec
init = do
  _ <- logInfo "StreamRelay Supervisor starting" {}
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
