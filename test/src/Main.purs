module Main where

import Prelude

import Control.Monad.State (evalStateT, lift)
import Cases.Startup (startupTests)
import Cases.Ingest (ingestTests)
import Data.Array (delete)
import Data.Array as Array
import Data.Identity (Identity(..))
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Helpers.Assert
import Helpers.CreateString (toAddrFromNode)
import Helpers.Env
import Helpers.Functions
import Helpers.HTTP as HTTP
import Helpers.Log (as, as', asT, asT')
import Helpers.Types (Node, TestNode, ToRecord)
import Helpers.OsCmd (runProc)
import Shared.Chaos as Chaos
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Stream (SlotRole(..))
import Shared.Types.Agent.State as PublicState
import Test.Spec (after_, before_, describe, describeOnly, it, itOnly)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT)


main :: Effect Unit
main =
  launchAff_ $ un Identity $ runSpecT testConfig [consoleReporter] do
    startupTests
    ingestTests


    describe "3 Ingest egest tests" do
      describe "3.1 one pop setup"
        let
          p1Nodes = [p1n1, p1n2, p1n3]
          nodes = p1Nodes
          allNodesBar node = delete node nodes
        in do
        before_ (do
                   startSession nodes
                   launch nodes
                ) do
          after_ stopSession do
            it "3.1.1 client requests stream on ingest node" do
                HTTP.clientStart p1n1 slot1           >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
                HTTP.getRelayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as  "no relay prior to ingest"
                HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
                waitForAsyncProfileStart                                  >>= as' "wait for async start of profile"
                HTTP.clientStart p1n1 slot1           >>= assertStatusCode 204 >>= as  "egest available"
                waitForAsyncProfileStart                                  >>= as' "wait for async start of profile"
                HTTP.getRelayStats   p1n1 slot1          >>= assertStatusCode 200
                                                     >>= assertRelayForEgest [p1n1]
                                                                          >>= as  "local relay exists"
                HTTP.getEgestStats   p1n1 slot1          >>= assertStatusCode 200
                                                     >>= assertEgestClients 1
                                                                          >>= as "agent should have 1 client"
            it "3.1.2 client requests stream on non-ingest node" do
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
              HTTP.getRelayStats   p1n2 slot1          >>= assertStatusCode 404 >>= as  "no remote relay prior to ingest"
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP source avaialable to disseminate"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "egest available"
              HTTP.getRelayStats   p1n2 slot1          >>= assertStatusCode 200 >>= as  "remote relay exists"
              HTTP.getEgestStats   p1n2 slot1          >>= assertStatusCode 200
                                                   >>= assertEgestClients 1
                                                                        >>= as "agent should have 1 client"

            it "3.1.3 client requests stream on 2nd node on ingest pop" do
              (flip evalStateT) Map.empty $ do
                lift $ HTTP.clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as "no egest p1n2 prior to ingest"
                lift $ HTTP.clientStart p1n3 slot1           >>= assertStatusCode 404 >>= as "no egest p1n3 prior to ingest"
                lift $ HTTP.ingestStart p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "create ingest"
                lift $ waitForIntraPoPDisseminate                                >>= as' "allow intraPoP source avaialable to disseminate"
                (lift $ HTTP.clientStart p1n2 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId1"
                                                                                 >>= asT "first egest is same node"
                lift $ waitForIntraPoPDisseminate                                >>= as' "allow intraPoP egest avaialable to disseminate"
                (lift $ HTTP.clientStart p1n3 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId2"
                                                                                 >>= asT "p1n3 egest redirects to p1n2"
                lift $ HTTP.getEgestStats   p1n2 slot1          >>= assertStatusCode 200
                                                        >>= assertEgestClients 2
                                                                                 >>= as "agent should have 2 clients"
                lift $ HTTP.getEgestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "no egest on node3"
                (lift $ HTTP.clientStart p1n2 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId3"
                                                                                 >>= asT "p1n2 stays on node2"
                (lift $ HTTP.clientStart p1n3 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId4"
                                                                                 >>= asT "p1n3 egest still redirects to p1n2"
                lift $ HTTP.getEgestStats   p1n2 slot1          >>= assertStatusCode 200
                                                        >>= assertEgestClients 4
                                                                                 >>= as "agent now has 4 clients"
                lift $ HTTP.getEgestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "still no egest on node3"
                clientId1 <- getStateValue "clientId1" "unknown"
                lift $ HTTP.clientStop clientId1  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 1 on node2"
                clientId2 <- getStateValue "clientId2" "unknown"
                lift $ HTTP.clientStop clientId2  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 2 on node2"
                clientId3 <- getStateValue "clientId3" "unknown"
                lift $ HTTP.clientStop clientId3  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 3 on node2"
                clientId4 <- getStateValue "clientId4" "unknown"
                lift $ HTTP.clientStop clientId4  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 4 on node2"

                lift $ waitForMoreThanEgestLinger                                >>= as' "allow the egest linger timer to expire"
                lift $ HTTP.getEgestStats   p1n2 slot1          >>= assertStatusCode 404 >>= as "now no egest on node2"
                lift $ HTTP.getEgestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "still no egest on node3"
                lift $ HTTP.clientStart p1n3 slot1           >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.3")
                                                                                 >>= as "Final egest starts on node3"
                lift $ HTTP.getEgestStats   p1n3 slot1          >>= assertStatusCode 200
                                                        >>= assertEgestClients 1
                                                                                 >>= as "node 3 agent should have 1 client"

      describe "3.2 two pop setup" do
        let
          p1Nodes = [p1n1, p1n2, p1n3]
          p2Nodes = [p2n1, p2n2]
          nodes = p1Nodes <> p2Nodes
        before_ (do
                   startSession nodes
                   launch nodes
                ) do
          after_ stopSession do
            it "3.2.1 aggregator presence is disseminated to all servers" do
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200      >>= as  "create ingest"
              waitForTransPoPDisseminate                                    >>= as' "wait for transPop disseminate"
              HTTP.getIntraPoPState p1n1                   >>= assertAggregatorOn [p1n1] slot1
                                                                            >>= as "p1n1 is aware of the ingest on p1n1"
              states1 <- traverse forceGetState (Array.toUnfoldable p1Nodes)
              assertSame states1                                            >>= as "All pop 1 nodes agree on leader and aggregator presence"
              states2 <- traverse forceGetState (Array.toUnfoldable p2Nodes)
              assertSame states2                                            >>= as "All pop 2 nodes agree on leader and aggregator presence"

            it "3.2.2 client requests stream on other pop" do
              HTTP.clientStart p2n1 slot1           >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
              HTTP.getRelayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as  "no remote relay prior to ingest"
              HTTP.getRelayStats   p2n1 slot1          >>= assertStatusCode 404 >>= as  "no local relay prior to ingest"
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              HTTP.clientStart p2n1 slot1           >>= assertStatusCode 204 >>= as  "egest available"
              HTTP.getRelayStats   p2n1 slot1          >>= assertStatusCode 200 >>= as  "local relay exists"
              waitForAsyncRelayStart                                    >>= as' "wait for the relay chain to start"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP to spread location of relay"
              HTTP.getIntraPoPState p1n1               >>= assertStatusCode 200
                                                   >>= assertRelayCount slot1 1
                                                                        >>= as  "relay created in aggregator pop"
              HTTP.getProxiedRelayStats p1n1 slot1     >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p2n1]
                                                                        >>= as  "remote relay is serving local relay"

            it "3.2.3 client ingest starts and stops" do
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "no local egest prior to ingest"
              HTTP.clientStart p2n1 slot1           >>= assertStatusCode 404 >>= as  "no remote egest prior to ingest"
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "local egest post ingest"
              HTTP.clientStart p2n1 slot1           >>= assertStatusCode 204 >>= as  "remote egest post ingest"
              HTTP.ingestStop  p1n1 slot1 low >>= assertStatusCode 200 >>= as  "stop the ingest"
              waitForTransPoPStopDisseminate                            >>= as' "wait for transPop disseminate"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "no same pop egest post stop"
              HTTP.clientStart p2n1 slot1           >>= assertStatusCode 404 >>= as  "no remote pop egest post stop"
              -- TODO - assert the relays stop as well - might be slow with timeouts chaining...


      describe "3.3 node startup - one pop" do
        let
          phase1Nodes = [p1n1, p1n2]
          phase2Nodes = [p1n3]
          nodes = phase1Nodes <> phase2Nodes
          sysconfig = "test/config/partial_nodes/sys.config"
        before_ (do
                   startSession nodes
                   launch' phase1Nodes sysconfig
                ) do
          after_ stopSession do
            it "3.3.1 a node that starts late gets to see existing streams" do
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "let ingest presence disseminate"
              launch' phase2Nodes sysconfig                             >>= as' "start new node after ingest already running"
              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"
              HTTP.clientStart p1n3 slot1           >>= assertStatusCode 204 >>= as  "local egest post ingest"

            -- TODO - egest - test stream we think is not present when it is

      describe "3.4 packet loss - one pop" do
        let
          nodes = [p1n1, p1n2]
          sysconfig = "test/config/partial_nodes/sys.config"
        before_ (do
                   startSession nodes
                   launch' nodes sysconfig
                ) do
          after_ stopSession do
            it "3.4.1 aggregator expired after extended packet loss" do
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "let ingest presence disseminate"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "local egest post ingest"
              HTTP.dropAgentMessages p1n2 true                               >>= as  "Drop all agent messages"
              waitForIntraPoPDisseminate                                >>= as' "Wait for less than message expiry"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "Initially clients can still join"
              waitForMessageTimeout                                     >>= as' "Wait for less than message expiry"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "Clients can no longer join"
              HTTP.dropAgentMessages p1n2 false                              >>= as  "Alow messages to flow once more"

              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"
              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"

              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "Client can join once more"

      describe "3.5 four pop setup" do
        let
          p1Nodes = [p1n1]  -- iad
          p2Nodes = [p2n1]  -- dal
          p3Nodes = [p3n1]  -- fra
          p4Nodes = [p4n1]  -- lax
          -- the topology in wanDefinition.json is important - maybe make explicit for this test...
          -- todo - why don't singleton pops work?
          nodes = p1Nodes <> p2Nodes <> p3Nodes <> p4Nodes

        before_ (do
                   startSession nodes
                   launch nodes
                ) do
          after_ stopSession do
            it "3.5.1 lax -> fra sets up 2 non-overlapping relay chains" do
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP to spread location of relay"
              HTTP.ingestStart p3n1 shortName1  low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              HTTP.clientStart p4n1 slot1           >>= assertStatusCode 204 >>= as  "egest available in lax"
              HTTP.getRelayStats  p4n1 slot1           >>= assertStatusCode 200 >>= as  "local relay exists"
              waitForAsyncRelayStart                                    >>= as' "wait for the relay chain to start"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP to spread location of relay"
              HTTP.getRelayStats p1n1 slot1            >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p4n1]
                                                       >>= assertRelayForEgest []
                                                                        >>= as  "iad relays for lax with no egests of its own"

              HTTP.getRelayStats p2n1 slot1            >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p4n1]
                                                       >>= assertRelayForEgest []
                                                                        >>= as  "dal relays for lax with no egests of its own"
              HTTP.getRelayStats p3n1 slot1            >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p1n1, p2n1]
                                                       >>= assertRelayForEgest []
                                                                        >>= as  "fra relays for both iad and dal with no egests of its own"

    describe "4 resilience" do
      let
        p1Nodes = [p1n1, p1n2, p1n3]
        p2Nodes = [p2n1, p2n2]
        p3Nodes = [p3n1]
        nodes = p1Nodes <> p2Nodes <> p3Nodes
        allNodesBar node = delete node nodes
        maxOut server = HTTP.setLoad server 60.0 >>= assertStatusCode 204 >>= as ("set load on " <> toAddrFromNode server)
        sysconfig = "test/config/partial_nodes/sys.config"
      before_ (do
                 startSession nodes
                 launch' nodes sysconfig
              ) do
        after_ stopSession do
          it "4.1 Launch ingest, terminate ingest aggregator process, new ingest aggregator continues to pull from ingest" do
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create ingest"
            waitForAsyncProfileStart                                     >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator has low only"
            HTTP.killProcessNode p1n1 (Chaos.defaultKill $ ingestAggregatorName slot1 Primary)
                                                >>=  assertStatusCode 204 >>= as "kill process"
            waitForSupervisorRecovery                                     >>= as' "wait for supervisor"
            HTTP.getAggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator still has low"

          it "4.2 Launch ingest with local aggregator, terminate ingest process, ingest aggregator removes ingest from list of active ingests" do
            HTTP.ingestStart    p1n1 shortName1 low >>= assertStatusCode 200   >>= as  "create ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n1 slot1         >>= assertStatusCode 200
                                                   >>= assertAggregator [low]
                                                                          >>= as  "aggregator has low only"
            HTTP.killProcessNode p1n1 (Chaos.defaultKill $ ingestName slot1 Primary "500")
                                               >>=  assertStatusCode 204 >>= as "kill process"
            waitForSupervisorRecovery                                    >>= as' "wait for supervisor"
            HTTP.getAggregatorStats p1n1 slot1         >>= assertStatusCode 200
                                                   >>= assertAggregator []
                                                                         >>= as  "aggregator has no ingests"

          it "4.3 Launch ingest with remote aggregator, terminate ingest process, ingest aggregator removes ingest from list of active ingests" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                          >>= as  "aggregator has low only"
            HTTP.killProcessNode p1n1 (Chaos.defaultKill $ ingestName slot1 Primary "500")
                                                >>=  assertStatusCode 204 >>= as "kill process"
            waitForSupervisorRecovery                                     >>= as' "wait for supervisor"
            HTTP.getAggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                          >>= as  "aggregator has no ingests"

          it "4.4 Launch ingest with remote aggregator, terminate ingest node, ingest aggregator removes ingest from list of active ingests" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator has low only"
            stopNode p1n1                                                >>= as' "stop ingest node" -- no sleep needed, since stopNode takes time...
            HTTP.getAggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                         >>= as  "aggregator has no ingests"

          it "4.5 Launch ingest and egest, kill origin relay, assert slot state is still valid" do
            (flip evalStateT) Map.empty $ do
              lift $ HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
              lift $ waitForTransPoPDisseminate                                    >>= as' "wait for async start of ingest"
              lift $ HTTP.clientStart p2n1 slot1              >>= assertStatusCode 204  >>= as  "egest available"
              lift $ waitForTransPoPDisseminate                                    >>= as' "wait for async start of egest"
              (lift $ HTTP.getSlotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
                                                         <#> ((<$>) canonicaliseSlotState))
                                                         >>= storeSlotState        >>= asT "stored state"
              killOriginRelay slot1 Primary                                        >>= asT' "kill origin relay"
              lift $ waitForAsyncProfileStart                                      >>= as' "wait for recovery"
              (lift $ HTTP.getSlotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
                                                         <#> ((<$>) canonicaliseSlotState))
                                                         >>= compareSlotState excludePorts (==)
                                                         >>= compareSlotState identity (/=)
                                                                                   >>= asT "compare state"

          it "4.6 Launch ingest and egest, kill downstream relay, assert slot state is still valid" do
            (flip evalStateT) Map.empty $ do
              lift $ HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
              lift $ waitForTransPoPDisseminate                                    >>= as' "wait for async start of ingest"
              lift $ HTTP.clientStart p2n1 slot1              >>= assertStatusCode 204  >>= as  "egest available"
              lift $ waitForTransPoPDisseminate                                    >>= as' "wait for async start of egest"
              (lift $ HTTP.getSlotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
                                                         <#> ((<$>) (excludePorts <<< canonicaliseSlotState)))
                                                         >>= storeSlotState        >>= asT "stored state"
              killDownstreamRelay slot1 Primary                                    >>= asT' "kill downstream relay"
              lift $ waitForAsyncProfileStart                                      >>= as' "wait for recovery"
              (lift $ HTTP.getSlotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
                                                         <#> ((<$>) canonicaliseSlotState))
                                                         >>= compareSlotState excludePorts (==)
                                                         >>= compareSlotState identity (/=)
                                                                                   >>= asT "compare state"

    describe "Cleanup" do
      after_ stopSession do
        it "final cleanup" do
          pure unit


  where
    testConfig = { slow: Milliseconds 5000.0, timeout: Just (Milliseconds 25000.0), exit: false }

