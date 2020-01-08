module Rtsv2.Agents.EgestInstanceSup
       ( startLink
       , maybeStartAndAddClient
       , isAvailable
       )
       where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, nil, singleton, (:))
import Logger (Logger)
import Logger as Logger
import Pinto (SupervisorName)
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Names as Names
import Shared.Stream (StreamId)

serverName :: SupervisorName
serverName = Names.egestInstanceSupName

isAvailable :: Effect Boolean
isAvailable = Names.isRegistered serverName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

maybeStartAndAddClient :: StreamId -> Effect Unit
maybeStartAndAddClient streamId = do
  isActive <- EgestInstance.isActive streamId
  case isActive of
    false -> do
             _ <- Sup.startSimpleChild childTemplate serverName streamId
             maybeStartAndAddClient streamId
    true ->
      do
        _ <- EgestInstance.addClient streamId
        pure unit

  -- result <- Sup.startSimpleChild childTemplate serverName streamId
  -- case result of
  --   Pinto.AlreadyStarted pid -> pure unit
  --   Pinto.Started pid -> pure unit

init :: Effect Sup.SupervisorSpec
init = do
  _ <- logInfo "Egest Supervisor starting" {}
  pure $ Sup.buildSupervisor
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

childTemplate :: Pinto.ChildTemplate StreamId
childTemplate = Pinto.ChildTemplate (EgestInstance.startLink)

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = serverName # Names.toDomain # singleton

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
