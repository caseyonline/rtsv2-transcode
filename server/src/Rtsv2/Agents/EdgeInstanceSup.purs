module Rtsv2.Agents.EdgeInstanceSup
       ( startLink
       , maybeStartAndAddClient
       , isAvailable
       )
       where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Utils as ErlUtils
import Foreign (Foreign)
import Logger as Logger
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Record as Record
import Rtsv2.Agents.EdgeInstance as EdgeInstance
import Rtsv2.Names as Names
import Shared.Agent as Agent
import Shared.Stream (StreamId)

serverName :: String
serverName = Names.edgeAgentSupName

isAvailable :: Effect Boolean
isAvailable = ErlUtils.isRegistered serverName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

maybeStartAndAddClient :: StreamId -> Effect Unit
maybeStartAndAddClient streamId = do
  isActive <- EdgeInstance.isActive streamId
  case isActive of
    false -> do
             _ <- Sup.startSimpleChild childTemplate serverName streamId
             maybeStartAndAddClient streamId
    true ->
      do
        _ <- EdgeInstance.addClient streamId
        pure unit

  -- result <- Sup.startSimpleChild childTemplate serverName streamId
  -- case result of
  --   Pinto.AlreadyStarted pid -> pure unit
  --   Pinto.Started pid -> pure unit

init :: Effect Sup.SupervisorSpec
init = do
  _ <- logInfo "Edge Supervisor starting" {}
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.SimpleOneForOne
    # Sup.supervisorChildren
        ( ( buildChild
              # childType Worker
              # childId "edgeAgent"
              # childStartTemplate childTemplate
              # childRestart Transient
          )
            : nil
        )

childTemplate :: Pinto.ChildTemplate StreamId
childTemplate = Pinto.ChildTemplate (EdgeInstance.startLink)

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Edge)) : nil) } { misc: metaData })
