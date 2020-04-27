module Rtsv2.Agents.StreamRelayInstanceSup
       ( startLink
       )
       where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Logger (Logger)
import Logger as Logger
import Pinto as Pinto
import Pinto.Sup (SupervisorChildType(..), SupervisorStrategy(..), buildChild, childId, childStart, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload)
import Rtsv2.Names as Names
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.Stream (RelayKey)

startLink :: RelayKey -> CreateRelayPayload -> StreamRelayInstance.StateServerName -> Effect Pinto.StartLinkResult
startLink relayKey payload stateServerName = Sup.startLink (Names.streamRelayInstanceSupName relayKey) (init relayKey payload stateServerName)

init :: RelayKey -> CreateRelayPayload -> StreamRelayInstance.StateServerName -> Effect Sup.SupervisorSpec
init relayKey payload stateServerName = do
  logInfo "StreamRelay Supervisor starting" {}
  pure $ Sup.buildSupervisor
    # Sup.supervisorIntensity 50
    # Sup.supervisorStrategy OneForAll
    # Sup.supervisorChildren
        ( ( buildChild
              # childType Worker
              # childId "streamRelayAgentInstance"
              # childStart (StreamRelayInstance.startLink relayKey payload) stateServerName
          )
            : nil
        )

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.StreamRelay :  "Instance" : nil)

logInfo :: forall a. Logger (Record a)
logInfo = domainLog Logger.info

domainLog :: forall a. Logger {domain :: List Atom, misc :: Record a} -> Logger (Record a)
domainLog = Logger.doLog domains
