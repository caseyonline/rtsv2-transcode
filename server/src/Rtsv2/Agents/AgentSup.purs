module Rtsv2.Agents.AgentSup where

import Prelude

import Data.Traversable (sequence)
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
  agentSpecs <- sequence $ (makeSpec <$> (IntraPoP : nodeConfig.agents))
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy OneForOne
    # Sup.supervisorChildren agentSpecs

  where
    makeSpec :: Agent -> Effect SupervisorChildSpec
    makeSpec Edge =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "edgeAgent"
        # Sup.childStart EdgeInstanceSup.startLink unit
        # pure

    makeSpec Ingest =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "ingestAgent"
        # Sup.childStart IngestSup.startLink unit
        # pure

    makeSpec IngestAggregator =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "ingestAggregatorAgent"
        # Sup.childStart IngestAggregatorInstanceSup.startLink unit
        # pure

    makeSpec StreamRelay =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "streamRelayAgent"
        # Sup.childStart StreamRelayInstanceSup.startLink unit
        # pure

    makeSpec IntraPoP = do
      intraPoPAgentConfig <- Config.intraPoPAgentConfig
      Sup.buildChild
        # Sup.childType Worker
        # Sup.childId "intraPopAgent"
        # Sup.childStart IntraPoP.startLink { config: intraPoPAgentConfig
                                            , transPoPApi: { announceStreamIsAvailable: TransPoP.announceStreamIsAvailable
                                                           , announceStreamStopped: TransPoP.announceStreamStopped
                                                           , handleRemoteLeaderAnnouncement: TransPoP.handleRemoteLeaderAnnouncement}
                                            }
        # pure

    makeSpec TransPoP = do
      transPoPAgentConfig <- Config.transPoPAgentConfig
      Sup.buildChild
        # Sup.childType Worker
        # Sup.childId "transPopAgent"
        # Sup.childStart TransPoP.startLink { config: transPoPAgentConfig
                                                 , intraPoPApi: { announceRemoteStreamIsAvailable: IntraPoP.announceRemoteStreamIsAvailable
                                                                , announceRemoteStreamStopped: IntraPoP.announceRemoteStreamStopped
                                                                , announceTransPoPLeader: IntraPoP.announceTransPoPLeader}
                                                 }
        # pure
