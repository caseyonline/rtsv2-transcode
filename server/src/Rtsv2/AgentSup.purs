module Rtsv2.AgentSup where

import Prelude
import Effect (Effect)
import Erl.Data.List ((:))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildSpec, SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..))
import Pinto.Sup as Sup
import Rtsv2.Config as Config
import Rtsv2.EdgeAgentSup as EdgeAgentSup
import Rtsv2.IngestAgentSup as IngestAgentSup
import Rtsv2.IngestAggregatorAgentSup as IngestAggregatorAgentSup
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.StreamRelayAgentSup as StreamRelayAgentSup
import Rtsv2.TransPoPAgent as TransPoPAgent
import Shared.Agent (Agent(..))

startLink :: Unit -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink "agentSup" init

init :: Effect SupervisorSpec
init = do
  nodeConfig <- Config.nodeConfig
  intraPoPAgentConfig <- Config.intraPoPAgentConfig
  let
    makeSpec :: Agent -> SupervisorChildSpec
    makeSpec Edge =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "edgeAgent"
        # Sup.childStart EdgeAgentSup.startLink unit

    makeSpec Ingest =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "ingestAgent"
        # Sup.childStart IngestAgentSup.startLink unit

    makeSpec IngestAggregator =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "ingestAggregatorAgent"
        # Sup.childStart IngestAggregatorAgentSup.startLink unit

    makeSpec StreamRelay =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "streamRelayAgent"
        # Sup.childStart StreamRelayAgentSup.startLink unit

    makeSpec IntraPoP =
      Sup.buildChild
        # Sup.childType Worker
        # Sup.childId "intraPopAgent"
        # Sup.childStart IntraPoPAgent.startLink intraPoPAgentConfig

    makeSpec TransPoP =
      Sup.buildChild
        # Sup.childType Worker
        # Sup.childId "transPopAgent"
        # Sup.childStart TransPoPAgent.startLink {}
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy OneForOne
    # Sup.supervisorChildren (makeSpec <$> (IntraPoP : nodeConfig.agents))
