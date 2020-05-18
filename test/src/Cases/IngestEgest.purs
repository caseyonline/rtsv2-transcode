module Cases.IngestEgest where

import Prelude

import Control.Monad.State (evalStateT, lift)
import Effect.Aff (Aff)
import Data.Array as Array
import Data.Map as Map
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Helpers.Assert as A
import Helpers.HTTP as HTTP
import Helpers.Env as E
import Helpers.Functions (startSession, launch, stopSession, launch', forceGetState, storeHeader, getStateValue)
import Helpers.Log (as, as', asT)
import Shared.Rtsv2.Types (Canary(..))
import Test.Spec (SpecT, after_, before_, describe, it, itOnly)


ingestEgestTests :: forall m. Monad m => SpecT Aff Unit m Unit
ingestEgestTests = do
  describe "3 Ingest egest tests" do
    onePoPSetup
    twoPoPSetup
    nodeStartupOnePoP
    packetLossOnePoP
    fourPoPSetup


onePoPSetup :: forall m. Monad m => SpecT Aff Unit m Unit
onePoPSetup = do
  describe "3.1 one pop setup" do
    let nodes = [E.p1n1, E.p1n2, E.p1n3]
    before_ (startSession nodes *> launch nodes) do
      after_ stopSession do
        it "3.1.1 client requests stream on ingest node" do
          HTTP.clientStart E.p1n1 Live E.slot1       >>= A.assertStatusCode 404 >>= as  "no egest prior to ingest"
          HTTP.getRelayStats E.p1n1 E.slot1          >>= A.assertStatusCode 404 >>= as  "no relay prior to ingest"
          HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName >>= A.assertStatusCode 200 >>= as  "create ingest"
          E.waitForAsyncProfileStart                                            >>= as' "wait for async start of profile"
          HTTP.clientStart E.p1n1 Live E.slot1       >>= A.assertStatusCode 204 >>= as  "egest available"
          E.waitForAsyncProfileStart                                            >>= as' "wait for async start of profile"
          HTTP.getRelayStats E.p1n1 E.slot1          >>= A.assertStatusCode 200
                                                     >>= A.assertRelayForEgest [E.p1n1]
                                                                                >>= as  "local relay exists"
          HTTP.getEgestStats E.p1n1 E.slot1          >>= A.assertStatusCode 200
                                                     >>= A.assertEgestClients 1 >>= as "agent should have 1 client"

        it "3.1.2 client requests stream on non-ingest node" do
          HTTP.clientStart E.p1n2 Live E.slot1       >>= A.assertStatusCode 404 >>= as  "no egest prior to ingest"
          HTTP.getRelayStats E.p1n2 E.slot1          >>= A.assertStatusCode 404 >>= as  "no remote relay prior to ingest"
          HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName >>= A.assertStatusCode 200 >>= as  "create ingest"
          E.waitForIntraPoPDisseminate               >>= as' "allow intraPoP source avaialable to disseminate"
          HTTP.clientStart E.p1n2 Live E.slot1       >>= A.assertStatusCode 204 >>= as  "egest available"
          HTTP.getRelayStats E.p1n2 E.slot1          >>= A.assertStatusCode 200 >>= as  "remote relay exists"
          HTTP.getEgestStats E.p1n2 E.slot1          >>= A.assertStatusCode 200
                                                     >>= A.assertEgestClients 1 >>= as "agent should have 1 client"

        it "3.1.3 client requests stream on 2nd node on ingest pop" do
          (flip evalStateT) Map.empty $ do
            lift $ HTTP.clientStart E.p1n2 Live E.slot1       >>= A.assertStatusCode 404 >>= as "no egest E.p1n2 prior to ingest"
            lift $ HTTP.clientStart E.p1n3 Live E.slot1       >>= A.assertStatusCode 404 >>= as "no egest E.p1n3 prior to ingest"
            lift $ HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName >>= A.assertStatusCode 200 >>= as "create ingest"
            lift $ E.waitForIntraPoPDisseminate                                          >>= as' "allow intraPoP source avaialable to disseminate"
            (lift $ HTTP.clientStart E.p1n2 Live E.slot1      >>= A.assertStatusCode 204
                                                              >>= A.assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                              >>= storeHeader "x-client-id" "clientId1"
                                                                                         >>= asT "first egest is same node"
            lift $ E.waitForIntraPoPDisseminate                                          >>= as' "allow intraPoP egest avaialable to disseminate"
            (lift $ HTTP.clientStart E.p1n3 Live E.slot1      >>= A.assertStatusCode 204
                                                              >>= A.assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                              >>= storeHeader "x-client-id" "clientId2"
                                                                                         >>= asT "E.p1n3 egest redirects to E.p1n2"
            lift $ HTTP.getEgestStats E.p1n2 E.slot1          >>= A.assertStatusCode 200
                                                              >>= A.assertEgestClients 2 >>= as "agent should have 2 clients"
            lift $ HTTP.getEgestStats E.p1n3 E.slot1          >>= A.assertStatusCode 404 >>= as "no egest on node3"
            (lift $ HTTP.clientStart E.p1n2 Live E.slot1      >>= A.assertStatusCode 204
                                                              >>= A.assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                              >>= storeHeader "x-client-id" "clientId3"
                                                              >>= asT "egest stays on node2"
            (lift $ HTTP.clientStart E.p1n3 Live E.slot1           >>= A.assertStatusCode 204
                                                              >>= A.assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                              >>= storeHeader "x-client-id" "clientId4"
                                                              >>= asT "egest still redirects to E.p1n2"
            lift $ HTTP.getEgestStats E.p1n2 E.slot1          >>= A.assertStatusCode 200
                                                              >>= A.assertEgestClients 4 >>= as "agent now has 4 clients"
            lift $ HTTP.getEgestStats   E.p1n3 E.slot1        >>= A.assertStatusCode 404 >>= as "still no egest on node3"
            clientId1 <- getStateValue "clientId1" "unknown"
            lift $ HTTP.clientStop clientId1  E.p1n2 E.slot1  >>= A.assertStatusCode 204 >>= as "stop client 1 on node2"
            clientId2 <- getStateValue "clientId2" "unknown"
            lift $ HTTP.clientStop clientId2  E.p1n2 E.slot1  >>= A.assertStatusCode 204 >>= as "stop client 2 on node2"
            clientId3 <- getStateValue "clientId3" "unknown"
            lift $ HTTP.clientStop clientId3  E.p1n2 E.slot1  >>= A.assertStatusCode 204 >>= as "stop client 3 on node2"
            clientId4 <- getStateValue "clientId4" "unknown"
            lift $ HTTP.clientStop clientId4  E.p1n2 E.slot1  >>= A.assertStatusCode 204 >>= as "stop client 4 on node2"

            lift $ E.waitForMoreThanEgestLinger                                          >>= as' "allow the egest linger timer to expire"
            lift $ HTTP.getEgestStats E.p1n2 E.slot1          >>= A.assertStatusCode 404 >>= as "now no egest on node2"
            lift $ HTTP.getEgestStats E.p1n3 E.slot1          >>= A.assertStatusCode 404 >>= as "still no egest on node3"
            lift $ HTTP.clientStart E.p1n3 Live E.slot1       >>= A.assertStatusCode 204
                                                              >>= A.assertHeader (Tuple "x-servedby" "172.16.169.3")
                                                                                         >>= as "Final egest starts on node3"
            lift $ HTTP.getEgestStats E.p1n3 E.slot1          >>= A.assertStatusCode 200
                                                              >>= A.assertEgestClients 1 >>= as "node 3 agent should have 1 client"


twoPoPSetup :: forall m. Monad m => SpecT Aff Unit m Unit
twoPoPSetup = do
  describe "3.2 two pop setup" do
    let p1Nodes = [E.p1n1, E.p1n2, E.p1n3]
        p2Nodes = [E.p2n1, E.p2n2]
        nodes = p1Nodes <> p2Nodes
    before_ (startSession nodes *> launch nodes) do
      after_ stopSession do
        it "3.2.1 aggregator presence is disseminated to all servers" do
          HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName >>= A.assertStatusCode 200 >>= as  "create ingest"
          E.waitForTransPoPDisseminate                                          >>= as' "wait for transPop disseminate"
          HTTP.getIntraPoPState E.p1n1               >>= A.assertAggregatorOn [E.p1n1] E.slot1
                                                                                >>= as "E.p1n1 is aware of the ingest on E.p1n1"
          states1 <- traverse forceGetState (Array.toUnfoldable p1Nodes)
          A.assertSame states1                                                  >>= as "All pop 1 nodes agree on leader and aggregator presence"
          states2 <- traverse forceGetState (Array.toUnfoldable p2Nodes)
          A.assertSame states2                                                  >>= as "All pop 2 nodes agree on leader and aggregator presence"

        it "3.2.2 client requests stream on other pop" do
          HTTP.clientStart E.p2n1 Live E.slot1       >>= A.assertStatusCode 404 >>= as  "no egest prior to ingest"
          HTTP.getRelayStats E.p1n1 E.slot1          >>= A.assertStatusCode 404 >>= as  "no remote relay prior to ingest"
          HTTP.getRelayStats E.p2n1 E.slot1          >>= A.assertStatusCode 404 >>= as  "no local relay prior to ingest"
          HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName >>= A.assertStatusCode 200 >>= as  "create ingest"
          E.waitForTransPoPDisseminate                                          >>= as' "wait for transPop disseminate"
          HTTP.clientStart E.p2n1 Live E.slot1       >>= A.assertStatusCode 204 >>= as  "egest available"
          HTTP.getRelayStats E.p2n1 E.slot1          >>= A.assertStatusCode 200 >>= as  "local relay exists"
          E.waitForAsyncRelayStart                                              >>= as' "wait for the relay chain to start"
          E.waitForIntraPoPDisseminate                                          >>= as' "allow intraPoP to spread location of relay"
          HTTP.getIntraPoPState E.p1n1               >>= A.assertStatusCode 200
                                                     >>= A.assertRelayCount E.slot1 1
                                                                                >>= as  "relay created in aggregator pop"
          HTTP.getProxiedRelayStats E.p1n1 E.slot1   >>= A.assertStatusCode 200
                                                     >>= A.assertRelayForRelay [E.p2n1]
                                                                                >>= as  "remote relay is serving local relay"

        it "3.2.3 client ingest starts and stops" do
          HTTP.clientStart E.p1n2 Live E.slot1            >>= A.assertStatusCode 404 >>= as  "no local egest prior to ingest"
          HTTP.clientStart E.p2n1 Live E.slot1            >>= A.assertStatusCode 404 >>= as  "no remote egest prior to ingest"
          HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName >>= A.assertStatusCode 200 >>= as  "create ingest"
          E.waitForTransPoPDisseminate                                          >>= as' "wait for transPop disseminate"
          HTTP.clientStart E.p1n2 Live E.slot1            >>= A.assertStatusCode 204 >>= as  "local egest post ingest"
          HTTP.clientStart E.p2n1 Live E.slot1            >>= A.assertStatusCode 204 >>= as  "remote egest post ingest"
          HTTP.ingestStop  E.p1n1 E.slot1 E.lowProfileName      >>= A.assertStatusCode 200 >>= as  "stop the ingest"
          E.waitForTransPoPStopDisseminate                                      >>= as' "wait for transPop disseminate"
          HTTP.clientStart E.p1n2 Live E.slot1            >>= A.assertStatusCode 404 >>= as  "no same pop egest post stop"
          HTTP.clientStart E.p2n1 Live E.slot1            >>= A.assertStatusCode 404 >>= as  "no remote pop egest post stop"
          -- TODO - A.assert the relays stop as well - might be slow with timeouts chaining...

nodeStartupOnePoP :: forall m. Monad m => SpecT Aff Unit m Unit
nodeStartupOnePoP =
  describe "3.3 node startup - one pop" do
    let phase1Nodes = [E.p1n1, E.p1n2]
        phase2Nodes = [E.p1n3]
        nodes = phase1Nodes <> phase2Nodes
        sysconfig = "test/config/partial_nodes/sys.config"
    before_ (startSession nodes *> launch' phase1Nodes sysconfig) do
      after_ stopSession do
        it "3.3.1 a node that starts late gets to see existing streams" do
          HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName >>= A.assertStatusCode 200 >>= as  "create ingest"
          E.waitForIntraPoPDisseminate                                          >>= as' "let ingest presence disseminate"
          launch' phase2Nodes sysconfig                                         >>= as' "start new node after ingest already running"
          E.waitForNodeStartDisseminate                                         >>= as' "let ingest presence disseminate"
          HTTP.clientStart E.p1n3 Live E.slot1            >>= A.assertStatusCode 204 >>= as  "local egest post ingest"

            -- TODO - egest - test stream we think is not present when it is


packetLossOnePoP :: forall m. Monad m => SpecT Aff Unit m Unit
packetLossOnePoP =
  describe "3.4 packet loss - one pop" do
    let nodes = [E.p1n1, E.p1n2]
        sysconfig = "test/config/partial_nodes/sys.config"
    before_ (startSession nodes *> launch' nodes sysconfig) do
      after_ stopSession do
        it "3.4.1 aggregator expired after extended packet loss" do
          HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName >>= A.assertStatusCode 200 >>= as  "create ingest"
          E.waitForIntraPoPDisseminate                                          >>= as' "let ingest presence disseminate"
          HTTP.clientStart E.p1n2 Live E.slot1            >>= A.assertStatusCode 204 >>= as  "local egest post ingest"
          HTTP.dropAgentMessages E.p1n2 true                                    >>= as  "Drop all agent messages"
          E.waitForIntraPoPDisseminate                                          >>= as' "Wait for less than message expiry"
          HTTP.clientStart E.p1n2 Live E.slot1            >>= A.assertStatusCode 204 >>= as  "Initially clients can still join"
          E.waitForMessageTimeout                                               >>= as' "Wait for less than message expiry"
          HTTP.clientStart E.p1n2 Live E.slot1            >>= A.assertStatusCode 404 >>= as  "Clients can no longer join"
          HTTP.dropAgentMessages E.p1n2 false                                   >>= as  "Alow messages to flow once more"
          E.waitForNodeStartDisseminate                                         >>= as' "let ingest presence disseminate"
          E.waitForNodeStartDisseminate                                         >>= as' "let ingest presence disseminate"
          HTTP.clientStart E.p1n2 Live E.slot1            >>= A.assertStatusCode 204 >>= as  "Client can join once more"


fourPoPSetup :: forall m. Monad m => SpecT Aff Unit m Unit
fourPoPSetup = do
  describe "3.5 four pop setup" do
    let p1Nodes = [E.p1n1]  -- iad
        p2Nodes = [E.p2n1]  -- dal
        p3Nodes = [E.p3n1]  -- fra
        p4Nodes = [E.p4n1]  -- lax
        -- the topology in wanDefinition.json is important - maybe make explicit for this test...
        -- TODO: - why don't singleton pops work?
        nodes = p1Nodes <> p2Nodes <> p3Nodes <> p4Nodes
    before_ (startSession nodes *> launch nodes) do
      after_ stopSession do
        it "3.5.1 lax -> fra sets up 2 non-overlapping relay chains" do
          E.waitForIntraPoPDisseminate                                          >>= as' "allow intraPoP to spread location of relay"
          HTTP.ingestStart E.p3n1 Live E.shortName1 E.lowStreamName >>= A.assertStatusCode 200 >>= as  "create ingest"
          E.waitForTransPoPDisseminate                                          >>= as' "wait for transPop disseminate"
          HTTP.clientStart E.p4n1 Live E.slot1            >>= A.assertStatusCode 204 >>= as  "egest available in lax"
          HTTP.getRelayStats  E.p4n1 E.slot1         >>= A.assertStatusCode 200 >>= as  "local relay exists"
          E.waitForAsyncRelayStart                                              >>= as' "wait for the relay chain to start"
          E.waitForIntraPoPDisseminate                                          >>= as' "allow intraPoP to spread location of relay"
          HTTP.getRelayStats E.p1n1 E.slot1          >>= A.assertStatusCode 200
                                                     >>= A.assertRelayForRelay [E.p4n1]
                                                     >>= A.assertRelayForEgest []
                                                                                >>= as  "iad relays for lax with no egests of its own"

          HTTP.getRelayStats E.p2n1 E.slot1          >>= A.assertStatusCode 200
                                                     >>= A.assertRelayForRelay [E.p4n1]
                                                     >>= A.assertRelayForEgest []
                                                                                >>= as  "dal relays for lax with no egests of its own"
          HTTP.getRelayStats E.p3n1 E.slot1          >>= A.assertStatusCode 200
                                                     >>= A.assertRelayForRelay [E.p1n1, E.p2n1]
                                                     >>= A.assertRelayForEgest []
                                                                                >>= as  "fra relays for both iad and dal with no egests of its own"
