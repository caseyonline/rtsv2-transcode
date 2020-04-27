module Rtsv2.Agents.EgestInstanceSup
       ( startLink
       , startEgest
       , maybeStartAndAddClient
       , isAgentAvailable
       )
       where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, nil, singleton, (:))
import Erl.Process.Raw (Pid)
import Logger (Logger)
import Logger as Logger
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.CachedInstanceState as CachedInstanceState
import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Names as Names
import Shared.Rtsv2.Stream (EgestKey(..))

serverName :: SupervisorName
serverName = Names.egestInstanceSupName

isAgentAvailable :: Effect Boolean
isAgentAvailable = Pinto.isRegistered serverName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startEgest :: CreateEgestPayload -> Effect Pinto.StartChildResult
startEgest payload@{slotId, slotRole} =
  let
    egestKey = EgestKey slotId slotRole
  in
    Sup.startSimpleChild childTemplate serverName { childStartLink: EgestInstance.startLink payload
                                                  , childStopAction: EgestInstance.stopAction egestKey
                                                  , serverName: Names.egestInstanceStateName egestKey
                                                  , domain: EgestInstance.domain
                                                  }

maybeStartAndAddClient :: Pid -> CreateEgestPayload -> Effect Unit
maybeStartAndAddClient pid payload = do
  let egestKey = (EgestKey payload.slotId payload.slotRole)
  isActive <- EgestInstance.isActive egestKey
  case isActive of
    false -> do
      _ <- startEgest payload
      maybeStartAndAddClient pid payload
    true ->
      do
        _ <- EgestInstance.addClient pid egestKey
        pure unit

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

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = serverName # Names.toDomain # singleton

logInfo :: forall a. Logger (Record a)
logInfo = Logger.doLog domain Logger.info

logWarning :: forall a. Logger (Record a)
logWarning = Logger.doLog domain Logger.warning

logStart :: forall a. Logger (Record a)
logStart = Logger.doLogEvent domain Logger.Start Logger.info
