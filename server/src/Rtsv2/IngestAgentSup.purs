module Rtsv2.IngestAgentSup
       ( startLink
       , startIngest
       )

       where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil)
import Gproc as Gproc
import Pinto as Pinto
import Pinto.Sup as Sup
import Rtsv2.IngestAgent as Ingest
import Shared.Agent as Agent

serverName :: String
serverName = "ingestAgentSup"

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startIngest :: Ingest.Config  -> Effect Unit
startIngest args = do
  result <- Sup.startSimpleChild childTemplate serverName args
  case result of
       Pinto.AlreadyStarted pid -> pure unit
       Pinto.Started pid -> pure unit


init :: Effect Sup.SupervisorSpec
init = do
  _ <- Gproc.register Agent.IngestAgent
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.OneForOne
    # Sup.supervisorChildren nil


childTemplate :: Pinto.ChildTemplate Ingest.Config
childTemplate = Pinto.ChildTemplate (Ingest.startLink)
