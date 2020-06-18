module Cases.Ingest (ingestTests) where

import Prelude

import Effect.Aff (Aff)
import Data.Traversable (traverse_)
import Helpers.Assert (assertStatusCode, assertAggregator, assertAggregatorOn)
import Helpers.Log (as, as')
import Helpers.HTTP as HTTP
import Helpers.Env as E
import Helpers.Functions (startSession, launch, stopSession, stopNode, maxOut, allNodesBar, aggregatorNotPresent)
import Helpers.Types (Node)
import Shared.Rtsv2.Types (CanaryState(..))
import Test.Spec (SpecT, after_, before_, describe, it, itOnly)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
ingestTests :: forall m. Monad m => SpecT Aff Unit m Unit
ingestTests = do
  describe "2 Ingest tests" do
    before_ (E.lookupPuppeteerEnv *> startSession nodes *> launch nodes) do
      after_ stopSession do
        ingestTests1
        ingestTests2
        ingestTests3
        ingestTests4
        ingestTests5
        ingestTests6
        ingestTests7
        ingestTests8
        ingestTests9
        ingestTests10

-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------
nodes :: Array Node
nodes = [E.p1n1, E.p1n2, E.p1n3]


-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------
ingestTests1 :: forall m. Monad m => SpecT Aff Unit m Unit
ingestTests1 =
  it "2.1 ingest aggregation created on ingest node" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                            >>= assertStatusCode 200 >>= as "create ingest"
    E.waitForAsyncProfileStart                                       >>= as' "wait for async start of profile"
    HTTP.getAggregatorStats E.p1n1 E.slot1  >>= assertStatusCode 200
                                            >>= assertAggregator [E.lowSlotAndProfileName]
                                                                     >>= as "aggregator has low only"

ingestTests2 :: forall m. Monad m => SpecT Aff Unit m Unit
ingestTests2 =
  it "2.2 2nd ingest doesn't start new aggregator since one is running" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 200 >>= as "create low ingest"
    E.waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.highStreamName
                                           >>= assertStatusCode 200 >>= as "create high ingest"
    E.waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.lowSlotAndProfileName, E.highSlotAndProfileName]
                                                                    >>= as "aggregator has 2 profiles"
ingestTests3 :: forall m. Monad m => SpecT Aff Unit m Unit
ingestTests3 =
  it "2.3 if ingest node is too loaded, then ingest aggregation starts on non-ingest node" do
    traverse_ maxOut (allNodesBar E.p1n2 nodes)                     >>= as' "load up all servers bar one"
    E.waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 200 >>= as "create low ingest"
    E.waitForIntraPoPDisseminate                                    >>= as' "allow remote ingest location to disseminate"
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.highStreamName
                                           >>= assertStatusCode 200 >>= as "create high ingest"
    E.waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
    HTTP.getAggregatorStats E.p1n2 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.lowSlotAndProfileName, E.highSlotAndProfileName]
                                                                    >>= as "aggregator is on E.p1n2"
ingestTests4 :: forall m. Monad m => SpecT Aff Unit m Unit
ingestTests4 =
  it "2.4 ingest on different node removes itself from aggregator when stopped" do
    traverse_ maxOut (allNodesBar E.p1n2 nodes)                     >>= as' "load up all servers bar one"
    E.waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 200 >>= as "create low ingest"
    E.waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
    HTTP.getAggregatorStats E.p1n2 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.lowSlotAndProfileName]
                                                                    >>= as "aggregator created on idle server"
    (traverse_ (aggregatorNotPresent E.slot1) $ allNodesBar E.p1n2 nodes)
                                                                    >>= as' "aggregator not on busy servers"
    HTTP.ingestStop E.p1n1 E.slot1 E.lowProfileName
                                           >>= assertStatusCode 200 >>= as "stop low ingest"
    E.waitForAsyncProfileStop                                       >>= as' "wait for async stop of profile"
    HTTP.getAggregatorStats E.p1n2 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator []  >>= as "aggregator has no profiles"

ingestTests5 :: forall m. Monad m => SpecT Aff Unit m Unit
ingestTests5 =
  it "2.5 ingest restarts aggregator if aggregator exits" do
    traverse_ maxOut (allNodesBar E.p1n2 nodes)                     >>= as' "load up all servers bar one"
    E.waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 200 >>= as "create low ingest"
    E.waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
    HTTP.getAggregatorStats E.p1n2 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.lowSlotAndProfileName]
                                                                    >>= as "aggregator created on idle server"
    traverse_ (aggregatorNotPresent E.slot1) (allNodesBar E.p1n2 nodes)
                                                                    >>= as' "aggregator not on busy servers"
    HTTP.setLoad E.p1n3 0.0                >>= assertStatusCode 204 >>= as "mark E.p1n3 as idle"
    stopNode E.p1n2                                                 >>= as' "make E.p1n2 fail"
    E.waitForNodeFailureDisseminate                                 >>= as' "allow failure to disseminate"
    HTTP.getAggregatorStats E.p1n3 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.lowSlotAndProfileName]
                                                                    >>= as "failed aggregator moved to new idle server"

ingestTests6 :: forall m. Monad m => SpecT Aff Unit m Unit
ingestTests6 =
  it "2.6 aggregator exits after last profile stops (with linger time)" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 200 >>= as "create low ingest"
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.highStreamName
                                           >>= assertStatusCode 200 >>= as "create high ingest"
    E.waitForAsyncProfileStart
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.lowSlotAndProfileName, E.highSlotAndProfileName]
                                                                    >>= as "aggregator has both profiles"

    HTTP.ingestStop E.p1n1 E.slot1 E.lowProfileName
                                           >>= assertStatusCode 200 >>= as "stop low ingest"
    E.waitForAsyncProfileStop
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.highSlotAndProfileName]
                                                                    >>= as "aggregator only has high"
    HTTP.ingestStop E.p1n1 E.slot1 E.highProfileName
                                           >>= assertStatusCode 200 >>= as "stop high ingest"
    E.waitForAsyncProfileStop
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator []  >>= as "aggregator has no profiles"
    E.waitForMoreThanLinger                                         >>= as' "wait for linger time"
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 404 >>= as "aggregator stops after linger"


ingestTests7 :: forall m. Monad m => SpecT Aff Unit m Unit
ingestTests7 =
  it "2.7 aggregator does not exit during linger time" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 200 >>= as "create low ingest"
    E.waitForAsyncProfileStart
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.lowSlotAndProfileName]
                                                                    >>= as "aggregator created"
    HTTP.ingestStop E.p1n1 E.slot1 E.lowProfileName
                                           >>= assertStatusCode 200 >>= as "stop low ingest"
    E.waitForAsyncProfileStop
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator []  >>= as "aggregator has no profiles"
    E.waitForLessThanLinger                                         >>= as' "wait for less than the linger time"
    HTTP.ingestStart E.p1n2 Live E.shortName1 E.highStreamName
                                           >>= assertStatusCode 200 >>= as "create high ingest on another node"
    E.waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.highSlotAndProfileName]
                                                                    >>= as "lingered aggregator has high profile"

ingestTests8 :: forall m. Monad m => SpecT Aff Unit m Unit
ingestTests8 =
  it "2.8 aggregator liveness detected on node stop" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 200 >>= as "create low ingest"
    E.waitForIntraPoPDisseminate
    HTTP.getIntraPoPState E.p1n1           >>= assertAggregatorOn [E.p1n1] E.slot1
                                                                    >>= as "E.p1n1 is aware of the ingest on E.p1n1"
    HTTP.getIntraPoPState E.p1n2           >>= assertAggregatorOn [E.p1n1] E.slot1
                                                                    >>= as "E.p1n2 is aware of the ingest on E.p1n1"
    stopNode E.p1n1                                                 >>= as' "make E.p1n1 fail"
    E.waitForNodeFailureDisseminate                                 >>= as' "allow failure to disseminate"
    HTTP.getIntraPoPState E.p1n2           >>= assertAggregatorOn [] E.slot1
                                                                    >>= as "E.p1n2 is aware the ingest stopped"

ingestTests9 :: forall m. Monad m => SpecT Aff Unit m Unit
ingestTests9 =
  it "2.9 attempt to ingest same profile twice on same node fails" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 200 >>= as "create low ingest"
    E.waitForIntraPoPDisseminate
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.lowSlotAndProfileName]
                                                                    >>= as "aggregator has low profile"
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 409 >>= as "2nd attempt to create low ingest fails"
    E.waitForIntraPoPDisseminate
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.lowSlotAndProfileName]
                                                                    >>= as "aggregator has single profile"

ingestTests10 :: forall m. Monad m => SpecT Aff Unit m Unit
ingestTests10 =
  it "2.10 attempt to ingest same profile twice on different node fails" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 200 >>= as "create low ingest"
    E.waitForIntraPoPDisseminate
    HTTP.getIntraPoPState E.p1n1           >>= assertAggregatorOn [E.p1n1] E.slot1
                                                                    >>= as "E.p1n1 is aware of the ingest on E.p1n1"
    HTTP.ingestStart E.p1n2 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 200 >>= as "2nd attempt to create low ingest succeeds (but actual create is async)"
    HTTP.getIntraPoPState E.p1n1           >>= assertAggregatorOn [E.p1n1] E.slot1
                                                                    >>= as "E.p1n1 is still aware of the ingest on E.p1n1"
    E.waitForIntraPoPDisseminate
    HTTP.ingestStop E.p1n2 E.slot1 E.lowProfileName
                                           >>= assertStatusCode 404 >>= as "stop ingest fails with 404 since async create failed"
    HTTP.ingestStop E.p1n1 E.slot1 E.lowProfileName
                                           >>= assertStatusCode 200 >>= as "stop initial ingest"
    E.waitForAsyncProfileStart
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator []
                                                                    >>= as "aggregator now has no profiles"

    HTTP.ingestStart E.p1n2 Live E.shortName1 E.lowStreamName
                                           >>= assertStatusCode 200 >>= as "final attempt to create low ingest succeeds"
    E.waitForIntraPoPDisseminate
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= assertStatusCode 200
                                           >>= assertAggregator [E.lowSlotAndProfileName]
                                                                    >>= as "aggregator now has low profile again"
