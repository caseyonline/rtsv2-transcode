module Rtsv2.Agents.AgentSup where

import Prelude

import Effect (Effect)
import Erl.Data.List ((:))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildSpec, SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..))
import Pinto.Sup as Sup
import Rtsv2.Agents.EdgeInstanceSup as EdgeInstanceSup
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Agents.IngestSup as IngestSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.StreamRelayInstanceSup as StreamRelayInstanceSup
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Shared.Agent (Agent(..))

startLink :: Unit -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink Names.agentSupName init

init :: Effect SupervisorSpec
init = do
  nodeConfig <- Config.nodeConfig
  intraPoPAgentConfig <- Config.intraPoPAgentConfig
  transPoPAgentConfig <- Config.transPoPAgentConfig
  let
    makeSpec :: Agent -> SupervisorChildSpec
    makeSpec Edge =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "edgeAgent"
        # Sup.childStart EdgeInstanceSup.startLink unit

    makeSpec Ingest =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "ingestAgent"
        # Sup.childStart IngestSup.startLink unit

    makeSpec IngestAggregator =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "ingestAggregatorAgent"
        # Sup.childStart IngestAggregatorInstanceSup.startLink unit

    makeSpec StreamRelay =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "streamRelayAgent"
        # Sup.childStart StreamRelayInstanceSup.startLink unit

    makeSpec IntraPoP =
      Sup.buildChild
        # Sup.childType Worker
        # Sup.childId "intraPopAgent"
        # Sup.childStart IntraPoP.startLink { config: intraPoPAgentConfig
                                            , transPoPApi: { announceStreamIsAvailable: TransPoP.announceStreamIsAvailable
                                                           , announceStreamStopped: TransPoP.announceStreamStopped
                                                           , announceTransPoPLeader: TransPoP.announceTransPoPLeader}
                                            }

    makeSpec TransPoP =
      Sup.buildChild
        # Sup.childType Worker
        # Sup.childId "transPopAgent"
        # Sup.childStart TransPoP.startLink { config: transPoPAgentConfig
                                                 , intraPoPApi: { announceRemoteStreamIsAvailable: IntraPoP.announceRemoteStreamIsAvailable
                                                                , announceRemoteStreamStopped: IntraPoP.announceRemoteStreamStopped
                                                                , announceTransPoPLeader: IntraPoP.announceTransPoPLeader}
                                                 }

  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy OneForOne
    # Sup.supervisorChildren (makeSpec <$> (IntraPoP : nodeConfig.agents))
