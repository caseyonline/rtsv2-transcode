module Rtsv2.Agents.StreamRelayInstanceSup
       ( startLink
       , startRelay
       )
       where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Foreign (Foreign)
import Logger as Logger
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Record as Record
import Rtsv2.Agents.StreamRelayInstance as StreamRelayInstance
import Rtsv2.Names as Names
import Shared.Agent as Agent
import Shared.Stream (StreamId)

serverName :: String
serverName = Names.streamRelayAgentSupName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startRelay :: StreamId -> Effect Unit
startRelay streamId = do
  result <- Sup.startSimpleChild childTemplate serverName streamId
  case result of
    Pinto.AlreadyStarted pid -> pure unit
    Pinto.Started pid -> pure unit

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

childTemplate :: Pinto.ChildTemplate StreamId
childTemplate = Pinto.ChildTemplate (StreamRelayInstance.startLink)

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.StreamRelay)) : nil) } { misc: metaData })
