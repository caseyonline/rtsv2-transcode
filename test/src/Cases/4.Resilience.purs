module Cases.Resilience where

import Prelude

import Control.Monad.State (evalStateT, lift)
import Data.Map as Map
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Helpers.Types (Node, ToRecord)
import Shared.Rtsv2.Agent.State as PublicState
import Helpers.Assert as A
import Helpers.Env as E
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log (as, as', asT, asT')
import Shared.Rtsv2.Stream (SlotRole(..))
import Shared.Rtsv2.Types (CanaryState(..))
import Shared.Rtsv2.Chaos as Chaos
import Test.Spec (SpecT, after_, before_, describe, it, itOnly)


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
resilienceTests :: forall m. Monad m => SpecT Aff Unit m Unit
resilienceTests = do
  describe "4 resilience" do
    before_ (E.lookupPuppeteerEnv *> F.startSession nodes *> F.launch' nodes "test/config/partial_nodes/sys.config") do
      after_ F.stopSession do
        resilienceTest1
        resilienceTest2
        resilienceTest3
        resilienceTest4
        resilienceTest5
        resilienceTest6
        resilienceTest7

-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------
nodes :: Array Node
nodes = [E.p1n1, E.p1n2, E.p1n3, E.p2n1, E.p2n2, E.p3n1]


-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------
resilienceTest1 :: forall m. Monad m => SpecT Aff Unit m Unit
resilienceTest1 =
  it "4.1 Launch ingest, terminate ingest aggregator process, new ingest aggregator continues to pull from ingest" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                               >>= A.assertStatusCode 200 >>= as  "create ingest"
    E.waitForAsyncProfileStart                                            >>= as' "wait for async start of profile"
    HTTP.getAggregatorStats E.p1n1 E.slot1     >>= A.assertStatusCode 200
                                               >>= A.assertAggregator [E.lowSlotAndProfileName]
                                                                          >>= as  "aggregator has low only"
    HTTP.killProcessNode E.p1n1 (Chaos.defaultKill $ E.ingestAggregatorName E.slot1 Primary)
                                               >>= A.assertStatusCode 204 >>= as "kill process"
    E.waitForSupervisorRecovery                                           >>= as' "wait for supervisor"
    HTTP.getAggregatorStats E.p1n1 E.slot1     >>= A.assertStatusCode 200
                                               >>= A.assertAggregator [E.lowSlotAndProfileName]
                                                                          >>= as "aggregator still has low"


resilienceTest2 :: forall m. Monad m => SpecT Aff Unit m Unit
resilienceTest2 =
  it "4.2 Launch ingest with local aggregator, terminate ingest process, ingest aggregator removes ingest from list of active ingests" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                               >>= A.assertStatusCode 200 >>= as  "create ingest"
    E.waitForAsyncProfileStart                                            >>= as' "wait for async start of profile"
    HTTP.getAggregatorStats E.p1n1 E.slot1     >>= A.assertStatusCode 200
                                               >>= A.assertAggregator [E.lowSlotAndProfileName]
                                                                          >>= as  "aggregator has low only"
    HTTP.killProcessNode E.p1n1 (Chaos.defaultKill $ E.ingestName E.slot1 Primary "low")
                                               >>= A.assertStatusCode 204 >>= as "kill process"
    E.waitForSupervisorRecovery                                           >>= as' "wait for supervisor"
    HTTP.getAggregatorStats E.p1n1 E.slot1     >>= A.assertStatusCode 200
                                               >>= A.assertAggregator []  >>= as  "aggregator has no ingests"



resilienceTest3 :: forall m. Monad m => SpecT Aff Unit m Unit
resilienceTest3 =
  it "4.3 Launch ingest with remote aggregator, terminate ingest process, ingest aggregator removes ingest from list of active ingests" do
    traverse_ F.maxOut (F.allNodesBar E.p1n2 nodes)                       >>= as' "load up all servers bar one"
    E.waitForIntraPoPDisseminate                                          >>= as' "allow load to disseminate"
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                               >>= A.assertStatusCode 200 >>= as  "create ingest"
    E.waitForAsyncProfileStart                                            >>= as' "wait for async start of profile"
    HTTP.getAggregatorStats E.p1n2 E.slot1     >>= A.assertStatusCode 200
                                               >>= A.assertAggregator [E.lowSlotAndProfileName]
                                                                          >>= as  "aggregator has low only"
    HTTP.killProcessNode E.p1n1 (Chaos.defaultKill $ E.ingestName E.slot1 Primary "low")
                                               >>= A.assertStatusCode 204 >>= as "kill process"
    E.waitForSupervisorRecovery                                           >>= as' "wait for supervisor"
    HTTP.getAggregatorStats E.p1n2 E.slot1     >>= A.assertStatusCode 200
                                               >>= A.assertAggregator []  >>= as  "aggregator has no ingests"

resilienceTest4 :: forall m. Monad m => SpecT Aff Unit m Unit
resilienceTest4 =
  it "4.4 Launch ingest with remote aggregator, terminate ingest node, ingest aggregator removes ingest from list of active ingests" do
    traverse_ F.maxOut (F.allNodesBar E.p1n2 nodes)                       >>= as' "load up all servers bar one"
    E.waitForIntraPoPDisseminate                                          >>= as' "allow load to disseminate"
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                               >>= A.assertStatusCode 200 >>= as  "create ingest"
    E.waitForAsyncProfileStart                                            >>= as' "wait for async start of profile"
    HTTP.getAggregatorStats E.p1n2 E.slot1     >>= A.assertStatusCode 200
                                               >>= A.assertAggregator [E.lowSlotAndProfileName]
                                                                          >>= as  "aggregator has low only"
    F.stopNode E.p1n1                                                     >>= as' "stop ingest node" -- no sleep needed, since stopNode takes time...
    HTTP.getAggregatorStats E.p1n2 E.slot1     >>= A.assertStatusCode 200
                                               >>= A.assertAggregator []  >>= as  "aggregator has no ingests"

resilienceTest5 :: forall m. Monad m => SpecT Aff Unit m Unit
resilienceTest5 =
  it "4.5 Launch ingest and egest, kill origin relay, Assert slot state is still valid" do
    (flip evalStateT) Map.empty $ do
      lift $ HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                               >>= A.assertStatusCode 200 >>= as  "create ingest"
      lift $ E.waitForTransPoPDisseminate                                 >>= as' "wait for async start of ingest"
      lift $ HTTP.clientStart E.p2n1 Live E.slot1   >>= A.assertStatusCode 204 >>= as  "egest available"
      lift $ E.waitForTransPoPDisseminate                                 >>= as' "wait for async start of egest"
      (lift $ HTTP.getSlotState E.p1n1 E.slot1 >>= (F.bodyToRecord :: ToRecord (PublicState.SlotState Array))
       <#> ((<$>) F.canonicaliseSlotState))    >>= F.storeSlotState       >>= asT "stored state"
      F.killOriginRelay E.slot1 Primary                                   >>= asT' "kill origin relay"
      lift $ E.waitForAsyncProfileStart                                   >>= as' "wait for recovery"
      (lift $ HTTP.getSlotState E.p1n1 E.slot1 >>= (F.bodyToRecord :: ToRecord (PublicState.SlotState Array))
       <#> ((<$>) F.canonicaliseSlotState))
        >>= A.compareSlotState (F.excludeTimestamps <<< F.excludePorts) (==)
        >>= A.compareSlotState identity (/=)
        >>= asT "compare state"

resilienceTest6 :: forall m. Monad m => SpecT Aff Unit m Unit
resilienceTest6 =
  it "4.6 Launch ingest and egest, kill downstream relay, assert slot state is still valid" do
    (flip evalStateT) Map.empty $ do
      lift $ HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                               >>= A.assertStatusCode 200 >>= as  "create ingest"
      lift $ E.waitForTransPoPDisseminate                                 >>= as' "wait for async start of ingest"
      lift $ HTTP.clientStart E.p2n1 Live E.slot1   >>= A.assertStatusCode 204 >>= as  "egest available"
      lift $ E.waitForTransPoPDisseminate                                 >>= as' "wait for async start of egest"
      (lift $ HTTP.getSlotState E.p1n1 E.slot1 >>= (F.bodyToRecord :: ToRecord (PublicState.SlotState Array))
       <#> ((<$>) (F.excludeTimestamps <<< F.excludePorts <<< F.canonicaliseSlotState)))
                                               >>= F.storeSlotState       >>= asT "stored state"
      F.killDownstreamRelay E.slot1 Primary                               >>= asT' "kill downstream relay"
      lift $ E.waitForAsyncProfileStart                                   >>= as' "wait for recovery"
      (lift $ HTTP.getSlotState E.p1n1 E.slot1 >>= (F.bodyToRecord :: ToRecord (PublicState.SlotState Array))
       <#> ((<$>) F.canonicaliseSlotState))
              >>= A.compareSlotState (F.excludeTimestamps <<< F.excludePorts) (==)
              >>= A.compareSlotState identity (/=)
              >>= asT "compare state"

resilienceTest7 :: forall m. Monad m => SpecT Aff Unit m Unit
resilienceTest7 =
  it "4.7 Launch ingest and egest, trigger egest sup children restart (via media gateway), assert slot state is still valid" do
    (flip evalStateT) Map.empty $ do
      lift $ HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                               >>= A.assertStatusCode 200 >>= as  "create ingest"
      lift $ E.waitForTransPoPDisseminate                                 >>= as' "wait for async start of ingest"
      lift $ HTTP.clientStart E.p2n1 Live E.slot1   >>= A.assertStatusCode 204 >>= as  "egest available"
      lift $ E.waitForTransPoPDisseminate                                 >>= as' "wait for async start of egest"
      (lift $ HTTP.getSlotState E.p1n1 E.slot1 >>= (F.bodyToRecord :: ToRecord (PublicState.SlotState Array))
       <#> ((<$>) (F.excludeTimestamps <<< F.excludePorts <<< F.canonicaliseSlotState)))
                                               >>= F.storeSlotState       >>= asT "stored state"
      lift $ F.killMediaGateway E.p2n1                                    >>= as' "kill media gateway"
      lift $ E.waitForSupervisorRecovery                                  >>= as' "wait for recovery"
      lift $ HTTP.clientStart E.p2n1 Live E.slot1   >>= A.assertStatusCode 204 >>= as  "egest available"
