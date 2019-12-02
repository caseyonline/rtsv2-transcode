module Rtsv2.IngestAggregatorAgentSup
       ( startLink
       , startAggregator
       )

       where

import Prelude

import Effect (Effect)
import Erl.Data.List (nil, (:))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildRestart(..), SupervisorChildType(..), buildChild, childId, childRestart, childStartTemplate, childType)
import Pinto.Sup as Sup
import Rtsv2.IngestAggregatorAgent as IngestAggregatorAgent

serverName :: String
serverName = "ingestAggregatorAgentSup"

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink serverName init

startAggregator :: IngestAggregatorAgent.Config  -> Effect Unit
startAggregator args = do
  result <- Sup.startSimpleChild childTemplate serverName args
  case result of
       Pinto.AlreadyStarted pid -> pure unit
       Pinto.Started pid -> pure unit


init :: Effect Sup.SupervisorSpec
init = do
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy Sup.SimpleOneForOne
    # Sup.supervisorChildren
        ( ( buildChild
              # childType Worker
              # childId "ingestAggregatorAgent"
              # childStartTemplate childTemplate
              # childRestart Transient
          )
          : nil
        )


childTemplate :: Pinto.ChildTemplate IngestAggregatorAgent.Config
childTemplate = Pinto.ChildTemplate (IngestAggregatorAgent.startLink)
