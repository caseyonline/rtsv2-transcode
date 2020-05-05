module Rtsv2.Agents.AgentSup where

import Prelude

import Data.Array (toUnfoldable)
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Data.List ((:))
import Pinto as Pinto
import Pinto.Sup (SupervisorChildSpec, SupervisorChildType(..), SupervisorSpec, SupervisorStrategy(..))
import Pinto.Sup as Sup
import Rtsv2.Agents.EgestSup as EgestSup
import Rtsv2.Agents.IngestAggregatorSup as IngestAggregatorSup
import Rtsv2.Agents.IngestSup as IngestSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.StreamRelaySup as StreamRelaySup
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Rtsv2.Agent (Agent(..))
import Shared.Rtsv2.Types (Server(..))

startLink :: Unit -> Effect Pinto.StartLinkResult
startLink _ = Sup.startLink Names.agentSupName init

init :: Effect SupervisorSpec
init = do
  (Server {agents}) <- PoPDefinition.getThisServer
  agentSpecs <- sequence $ (makeSpec <$> (IntraPoP : (toUnfoldable agents)))
  pure $ Sup.buildSupervisor
    # Sup.supervisorStrategy OneForOne
    # Sup.supervisorChildren agentSpecs

  where
    makeSpec :: Agent -> Effect SupervisorChildSpec
    makeSpec Egest =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "egestAgent"
        # Sup.childStart EgestSup.startLink unit
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
        # Sup.childStart IngestAggregatorSup.startLink unit
        # pure

    makeSpec StreamRelay =
      Sup.buildChild
        # Sup.childType Supervisor
        # Sup.childId "streamRelayAgent"
        # Sup.childStart StreamRelaySup.startLink unit
        # pure

    makeSpec IntraPoP = do
      intraPoPAgentConfig <- Config.intraPoPAgentConfig
      Sup.buildChild
        # Sup.childType Worker
        # Sup.childId "intraPopAgent"
        # Sup.childStart IntraPoP.startLink { config: intraPoPAgentConfig
                                            , transPoPApi: { announceAggregatorIsAvailable: TransPoP.announceAggregatorIsAvailable
                                                           , announceAggregatorStopped: TransPoP.announceAggregatorStopped
                                                           , handleRemoteLeaderAnnouncement: TransPoP.handleRemoteLeaderAnnouncement
                                                           }
                                            }
        # pure

    makeSpec TransPoP = do
      transPoPAgentConfig <- Config.transPoPAgentConfig
      Sup.buildChild
        # Sup.childType Worker
        # Sup.childId "transPopAgent"
        # Sup.childStart TransPoP.startLink { config: transPoPAgentConfig
                                            , intraPoPApi: { announceOtherPoPAggregatorIsAvailable: IntraPoP.announceOtherPoPAggregatorIsAvailable
                                                           , announceOtherPoPAggregatorStopped: IntraPoP.announceOtherPoPAggregatorStopped
                                                           , announceTransPoPLeader: IntraPoP.announceTransPoPLeader
                                                           }
                                            }
        # pure
