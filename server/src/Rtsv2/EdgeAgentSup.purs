module Rtsv2.EdgeAgentSup
       ( startLink
       , startEdge
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
import Rtsv2.EdgeAgent as Edge
import Shared.Agent as Agent
import Shared.Stream (StreamId(..))

serverName :: String
serverName = show Agent.Edge

isAvailable :: Effect Boolean
isAvailable = ErlUtils.isRegistered serverName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startEdge :: StreamId -> Effect Unit
startEdge streamId = do
  result <- Sup.startSimpleChild childTemplate serverName streamId
  case result of
    Pinto.AlreadyStarted pid -> pure unit
    Pinto.Started pid -> pure unit

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
childTemplate = Pinto.ChildTemplate (Edge.startLink)

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.Edge)) : nil) } { misc: metaData })
