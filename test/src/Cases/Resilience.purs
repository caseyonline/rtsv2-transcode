module Cases.Resilience where

import Prelude

import Control.Monad.State (evalStateT, lift)
import Data.Array (delete)
import Data.Map as Map
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Helpers.Types (ToRecord)
import Shared.Types.Agent.State as PublicState
import Helpers.Assert as A
import Helpers.CreateString (toAddrFromNode)
import Helpers.Env as E
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log (as, as', asT, asT')
import Shared.Stream (SlotRole(..))
import Shared.Chaos as Chaos
import Test.Spec (SpecT, after_, before_, describe, it)


resilienceTests :: forall m. Monad m => SpecT Aff Unit m Unit
resilienceTests = do
  describe "4 resilience" do
    let p1Nodes = [E.p1n1, E.p1n2, E.p1n3]
        p2Nodes = [E.p2n1, E.p2n2]
        p3Nodes = [E.p3n1]
        nodes = p1Nodes <> p2Nodes <> p3Nodes
        allNodesBar node = delete node nodes
        maxOut server = HTTP.setLoad server 60.0 >>= A.assertStatusCode 204 >>= as ("set load on " <> toAddrFromNode server)
        sysconfig = "test/config/partial_nodes/sys.config"
    before_ (F.startSession nodes *> F.launch' nodes sysconfig) do
      after_ F.stopSession do
        it "4.1 Launch ingest, terminate ingest aggregator process, new ingest aggregator continues to pull from ingest" do
          HTTP.ingestStart E.p1n1 E.shortName1 E.low  >>= A.assertStatusCode 200 >>= as  "create ingest"
          E.waitForAsyncProfileStart                                     >>= as' "wait for async start of profile"
          HTTP.getAggregatorStats E.p1n1 E.slot1          >>= A.assertStatusCode 200
                                                   >>= A.assertAggregator [E.low]
                                                                         >>= as  "aggregator has low only"
          HTTP.killProcessNode E.p1n1 (Chaos.defaultKill $ E.ingestAggregatorName E.slot1 Primary)
            >>=  A.assertStatusCode 204 >>= as "kill process"
          E.waitForSupervisorRecovery                                     >>= as' "wait for supervisor"
          HTTP.getAggregatorStats E.p1n1 E.slot1          >>= A.assertStatusCode 200
            >>= A.assertAggregator [E.low]
            >>= as  "aggregator still has low"

        it "4.2 Launch ingest with local aggregator, terminate ingest process, ingest aggregator removes ingest from list of active ingests" do
          HTTP.ingestStart    E.p1n1 E.shortName1 E.low >>= A.assertStatusCode 200   >>= as  "create ingest"
          E.waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
          HTTP.getAggregatorStats E.p1n1 E.slot1         >>= A.assertStatusCode 200
            >>= A.assertAggregator [E.low]
            >>= as  "aggregator has low only"
          HTTP.killProcessNode E.p1n1 (Chaos.defaultKill $ E.ingestName E.slot1 Primary "500")
            >>=  A.assertStatusCode 204 >>= as "kill process"
          E.waitForSupervisorRecovery                                    >>= as' "wait for supervisor"
          HTTP.getAggregatorStats E.p1n1 E.slot1         >>= A.assertStatusCode 200
            >>= A.assertAggregator []
            >>= as  "aggregator has no ingests"

        it "4.3 Launch ingest with remote aggregator, terminate ingest process, ingest aggregator removes ingest from list of active ingests" do
          traverse_ maxOut (allNodesBar E.p1n2)                           >>= as' "load up all servers bar one"
          E.waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
          HTTP.ingestStart    E.p1n1 E.shortName1 E.low  >>= A.assertStatusCode 200  >>= as  "create ingest"
          E.waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
          HTTP.getAggregatorStats E.p1n2 E.slot1          >>= A.assertStatusCode 200
            >>= A.assertAggregator [E.low]
            >>= as  "aggregator has low only"
          HTTP.killProcessNode E.p1n1 (Chaos.defaultKill $ E.ingestName E.slot1 Primary "500")
            >>=  A.assertStatusCode 204 >>= as "kill process"
          E.waitForSupervisorRecovery                                     >>= as' "wait for supervisor"
          HTTP.getAggregatorStats E.p1n2 E.slot1          >>= A.assertStatusCode 200
            >>= A.assertAggregator []
            >>= as  "aggregator has no ingests"

        it "4.4 Launch ingest with remote aggregator, terminate ingest node, ingest aggregator removes ingest from list of active ingests" do
          traverse_ maxOut (allNodesBar E.p1n2)                           >>= as' "load up all servers bar one"
          E.waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
          HTTP.ingestStart    E.p1n1 E.shortName1 E.low  >>= A.assertStatusCode 200  >>= as  "create ingest"
          E.waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
          HTTP.getAggregatorStats E.p1n2 E.slot1          >>= A.assertStatusCode 200
            >>= A.assertAggregator [E.low]
            >>= as  "aggregator has low only"
          F.stopNode E.p1n1                                                >>= as' "stop ingest node" -- no sleep needed, since stopNode takes time...
          HTTP.getAggregatorStats E.p1n2 E.slot1          >>= A.assertStatusCode 200
            >>= A.assertAggregator []
            >>= as  "aggregator has no ingests"

        it "4.5 Launch ingest and egest, kill origin relay, A.assert slot state is still valid" do
          (flip evalStateT) Map.empty $ do
            lift $ HTTP.ingestStart    E.p1n1 E.shortName1 E.low  >>= A.assertStatusCode 200  >>= as  "create ingest"
            lift $ E.waitForTransPoPDisseminate                                    >>= as' "wait for async start of ingest"
            lift $ HTTP.clientStart E.p2n1 E.slot1              >>= A.assertStatusCode 204  >>= as  "egest available"
            lift $ E.waitForTransPoPDisseminate                                    >>= as' "wait for async start of egest"
            (lift $ HTTP.getSlotState E.p1n1 E.slot1               >>= (F.bodyToRecord :: ToRecord (PublicState.SlotState Array))
             <#> ((<$>) F.canonicaliseSlotState))
              >>= F.storeSlotState        >>= asT "stored state"
            F.killOriginRelay E.slot1 Primary                                        >>= asT' "kill origin relay"
            lift $ E.waitForAsyncProfileStart                                      >>= as' "wait for recovery"
            (lift $ HTTP.getSlotState E.p1n1 E.slot1               >>= (F.bodyToRecord :: ToRecord (PublicState.SlotState Array))
             <#> ((<$>) F.canonicaliseSlotState))
              >>= A.compareSlotState F.excludePorts (==)
              >>= A.compareSlotState identity (/=)
              >>= asT "compare state"

        it "4.6 Launch ingest and egest, kill downstream relay, A.assert slot state is still valid" do
          (flip evalStateT) Map.empty $ do
            lift $ HTTP.ingestStart    E.p1n1 E.shortName1 E.low  >>= A.assertStatusCode 200  >>= as  "create ingest"
            lift $ E.waitForTransPoPDisseminate                                    >>= as' "wait for async start of ingest"
            lift $ HTTP.clientStart E.p2n1 E.slot1              >>= A.assertStatusCode 204  >>= as  "egest available"
            lift $ E.waitForTransPoPDisseminate                                    >>= as' "wait for async start of egest"
            (lift $ HTTP.getSlotState E.p1n1 E.slot1               >>= (F.bodyToRecord :: ToRecord (PublicState.SlotState Array))
             <#> ((<$>) (F.excludePorts <<< F.canonicaliseSlotState)))
              >>= F.storeSlotState        >>= asT "stored state"
            F.killDownstreamRelay E.slot1 Primary                                    >>= asT' "kill downstream relay"
            lift $ E.waitForAsyncProfileStart                                      >>= as' "wait for recovery"
            (lift $ HTTP.getSlotState E.p1n1 E.slot1               >>= (F.bodyToRecord :: ToRecord (PublicState.SlotState Array))
             <#> ((<$>) F.canonicaliseSlotState))
              >>= A.compareSlotState F.excludePorts (==)
              >>= A.compareSlotState identity (/=)
              >>= asT "compare state"
