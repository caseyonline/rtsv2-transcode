module Cases.Drain where

import Prelude

import Data.Traversable (traverse_)
import Debug.Trace (spy)
import Effect.Aff (Aff, delay, Milliseconds(..))
import Helpers.Assert as A
import Helpers.CreateString as C
import Helpers.Env as E
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log (as, as')
import Helpers.Types (Node)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Stream (SlotRole(..))
import Shared.Rtsv2.Types (CanaryState(..), RunState(..))
import Test.Spec (SpecT, after, after_, before, before_, describe, describeOnly, it, itOnly)
import Test.Unit.Assert as Assert
import Toppokki as T

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
drainTests :: forall m. Monad m => SpecT Aff Unit m Unit
drainTests =
  describe "9 Drain Tests" do
    passiveDrainTests
    forceDrainTests

passiveDrainTests :: forall m. Monad m => SpecT Aff Unit m Unit
passiveDrainTests = do
  describe "9.1 Passive Drain" do
    before_ (F.startSession twoNodes *> F.launch twoNodes) do
      after_ F.stopSession do
        passiveDrainTest1
        passiveDrainTest2
        passiveDrainTest3
        passiveDrainTest4

forceDrainTests :: forall m. Monad m => SpecT Aff Unit m Unit
forceDrainTests = do
  describe "9.2 Force Drain" do
    before_ (F.startSession threeNodes *> F.launch threeNodes) do
      after_ F.stopSession do
        forceDrainTest1
        forceDrainTest2

-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------
twoNodes :: Array Node
twoNodes = [E.p1n1, E.p1n2]

threeNodes :: Array Node
threeNodes = [E.p1n1, E.p1n2, E.p1n3]

options :: { args :: Array String
           , devtools :: Boolean
           , headless :: Boolean
           }
options =
  { headless: false
  , args: E.browserLaunchArgsIng
  , devtools: true
  }

-------------------------------------------------------------------------------
-- PassiveDrainTests.
-------------------------------------------------------------------------------
passiveDrainTest1 :: forall m. Monad m => SpecT Aff Unit m Unit
passiveDrainTest1 =
  it "9.1.1 Heathcheck endpoint returns correct run state" do
    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) Active <<< healthNodeToRunState)
                            >>= as "run state is active"
    pure unit

passiveDrainTest2 :: forall m. Monad m => SpecT Aff Unit m Unit
passiveDrainTest2 =
  it "9.1.2 Cannot create new agents on node once passive drain state entered, but can when switched back to active" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                       >>= A.assertStatusCode 200 >>= as "create low ingest"

    E.waitForAsyncProfileStart

    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 200
                                           >>= A.assertAggregator [E.lowSlotAndProfileName]
                                                                    >>= as "aggregator running with ingest connected"

    HTTP.changeRunState E.p1n1 PassiveDrain >>= A.assertStatusCode 204 >>= as "switch to passive drain"

    E.waitForAsyncProfileStart

    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 200
                                           >>= A.assertAggregator [E.lowSlotAndProfileName]
                                                                    >>= as "aggregator still running"

    HTTP.ingestStart E.p1n1 Live E.shortName1 E.highStreamName
                                       >>= A.assertStatusCode 409 >>= as "failed to create additional ingest"

    HTTP.changeRunState E.p1n1 Active >>= A.assertStatusCode 204 >>= as "switch to active"

    E.waitForAsyncProfileStart

    HTTP.ingestStart E.p1n1 Live E.shortName1 E.highStreamName
                                       >>= A.assertStatusCode 200 >>= as "high ingest created"

    E.waitForAsyncProfileStart

    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 200
                                           >>= A.assertAggregator [E.lowSlotAndProfileName, E.highSlotAndProfileName]
                                                                    >>= as "aggregator still running with both ingests"
    pure unit

passiveDrainTest3 :: forall m. Monad m => SpecT Aff Unit m Unit
passiveDrainTest3 =
  it "9.1.3 Once agents have passively drained, node switches to out-of-service state" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                       >>= A.assertStatusCode 200 >>= as "create low ingest"

    E.waitForAsyncProfileStart

    HTTP.changeRunState E.p1n1 PassiveDrain >>= A.assertStatusCode 204 >>= as "switch to passive drain"

    HTTP.ingestStop E.p1n1 E.slot1 E.lowProfileName
                                           >>= A.assertStatusCode 200 >>= as "stop low ingest"

    E.waitForMoreThanLinger                                         >>= as' "wait for linger time"

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) OutOfService <<< healthNodeToRunState)
                            >>= as "run state is out-of-service"

passiveDrainTest4 :: forall m. Monad m => SpecT Aff Unit m Unit
passiveDrainTest4 =
  it "9.1.4 Test valid and invalid transistions" do
    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) Active <<< healthNodeToRunState)
                            >>= as "run state is active"

    HTTP.changeRunState E.p1n1 PassiveDrain >>= A.assertStatusCode 204 >>= as "switch to passive drain"

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) PassiveDrain <<< healthNodeToRunState)
                            >>= as "run state is passiveDrain"

    HTTP.changeRunState E.p1n1 ForceDrain >>= A.assertStatusCode 204 >>= as "switch to force drain"

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) OutOfService <<< healthNodeToRunState)
                            >>= as "run state is outOfService (post forceDrain)"

    HTTP.changeRunState E.p1n1 Active >>= A.assertStatusCode 204 >>= as "switch to active"

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) Active <<< healthNodeToRunState)
                            >>= as "run state is active"

    HTTP.changeRunState E.p1n1 ForceDrain >>= A.assertStatusCode 204 >>= as "switch to force drain"

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) OutOfService <<< healthNodeToRunState)
                            >>= as "run state is outOfService (post forceDrain)"

    HTTP.changeRunState E.p1n1 Active >>= A.assertStatusCode 204 >>= as "switch to active"

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) Active <<< healthNodeToRunState)
                            >>= as "run state is active"

    HTTP.changeRunState E.p1n1 OutOfService >>= A.assertStatusCode 204 >>= as "switch to out-of-service"

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) OutOfService <<< healthNodeToRunState)
                            >>= as "run state is out-of-service"

    HTTP.changeRunState E.p1n1 PassiveDrain >>= A.assertStatusCode 409 >>= as "fail to switch from out-of-service to passive drain"

    HTTP.changeRunState E.p1n1 ForceDrain >>= A.assertStatusCode 409 >>= as "fail to switch from out-of-service to force drain"

    HTTP.changeRunState E.p1n1 Active >>= A.assertStatusCode 204 >>= as "switch to active"

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) Active <<< healthNodeToRunState)
                            >>= as "run state is active"

    HTTP.changeRunState E.p1n1 PassiveDrain >>= A.assertStatusCode 204 >>= as "switch to passive drain"

    HTTP.changeRunState E.p1n1 OutOfService >>= A.assertStatusCode 204 >>= as "switch to out-of-service"

    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) OutOfService <<< healthNodeToRunState)
                            >>= as "run state is out-of-service"

-- Move to out-of-service to close all existing agents

-------------------------------------------------------------------------------
-- PassiveDrainTests.
-------------------------------------------------------------------------------
forceDrainTest1 :: forall m. Monad m => SpecT Aff Unit m Unit
forceDrainTest1 =
  before (F.startSlotHigh1000 (C.toAddrFromNode E.p1n1) *> T.launch options) do
    after (\browser -> T.close browser *> F.stopSlot) do
      it "9.2.1 Force drain moves relay and egest agents to other node" $ \browser -> do

        E.waitForAsyncProfileStart

        HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 200
                                               >>= A.assertAggregator [E.highSlotAndProfileName]
                                                                        >>= as "aggregator running with ingest connected"
        E.waitForIntraPoPDisseminate               >>= as' "allow intraPoP source avaialable to disseminate"

        page <- T.newPage browser

        T.goto (HTTP.playerUrl E.p1n2 E.slot1 Primary) page
        delay (Milliseconds 3000.00) >>= as' "wait for video to start"

        HTTP.healthCheck E.p1n2 >>= A.assertStatusCode 200
                                >>= A.assertBodyFun ((==) 2 <<< healthNodeToAgentCount)
                                >>= as "two agents running on p1n2"

        HTTP.changeRunState E.p1n2 ForceDrain >>= A.assertStatusCode 204 >>= as "runState changed to force-drain"

        frames1 <- F.getInnerText "#frames" page
        packets1 <- F.getInnerText "#packets" page

        delay (Milliseconds 5000.00) >>= as' "let video play for 5 seconds"

        HTTP.healthCheck E.p1n2 >>= A.assertStatusCode 200
                                >>= A.assertBodyFun ((==) 0 <<< healthNodeToAgentCount)
                                >>= A.assertBodyFun ((==) OutOfService <<< healthNodeToRunState)
                                >>= as "no agents running on p1n2"

        frames2 <- F.getInnerText "#frames" page
        packets2 <- F.getInnerText "#packets" page

        let frameDiff = F.stringToInt frames2 - F.stringToInt frames1

        Assert.assert "frames aren't increasing" (frameDiff > 65) >>= as' ("frames increased by: " <> show frameDiff)
        Assert.assert "packets aren't increasing" ((F.stringToInt packets1) < (F.stringToInt packets2)) >>= as' ("packets are increasing: " <> packets2 <> " > " <> packets1)
        T.close browser

        pure unit

forceDrainTest2 :: forall m. Monad m => SpecT Aff Unit m Unit
forceDrainTest2 =
    before (T.launch options) do
    after (\browser -> T.close browser *> F.stopSlot) do
      it "9.2.2 Force drain moves aggregator, relay and egest agents to other node" $ \browser -> do

        traverse_ F.maxOut (F.allNodesBar E.p1n2 threeNodes)                   >>= as' "load up all servers bar one"
        F.startSlotHigh1000 (C.toAddrFromNode E.p1n1)                          >>= as' "start ingest on node 1"
        E.waitForAsyncProfileStart
        HTTP.getAggregatorStats E.p1n2 E.slot1 >>= A.assertStatusCode 200
                                               >>= A.assertAggregator [E.highSlotAndProfileName]
                                                                               >>= as "aggregator running on node 2 with ingest connected"
        E.waitForIntraPoPDisseminate                                           >>= as' "allow intraPoP source avaialable to disseminate"
        page <- T.newPage browser
        T.goto (HTTP.playerUrl E.p1n2 E.slot1 Primary) page
        delay (Milliseconds 3000.00)                                           >>= as' "wait for video to start from p1n2"
        HTTP.healthCheck E.p1n2 >>= A.assertStatusCode 200
                                >>= A.assertBodyFun ((==) 3 <<< healthNodeToAgentCount)
                                                                               >>= as "three agents running on p1n2"
        traverse_ F.freeUp [E.p1n3]                                            >>= as' "clear the load on all servers"
        E.waitForIntraPoPDisseminate                                           >>= as' "allow load change to disseminate"
        HTTP.changeRunState E.p1n2 ForceDrain >>= A.assertStatusCode 204       >>= as "runState changed to force-drain"
        delay (Milliseconds 5000.00)                                           >>= as' "allow force-drain to propogate"
        frames1 <- F.getInnerText "#frames" page
        packets1 <- F.getInnerText "#packets" page
        HTTP.healthCheck E.p1n2 >>= A.assertStatusCode 200
                                >>= A.assertBodyFun ((==) 0 <<< healthNodeToAgentCount)
                                >>= A.assertBodyFun ((==) OutOfService <<< healthNodeToRunState)
                                                                               >>= as "no agents running on p1n2"
        Assert.assert "packets received" (F.stringToInt packets1 > 0)          >>= as' ("initialialy received " <> packets1 <> " packets")
        Assert.assert "frames received" (F.stringToInt frames1 > 0)            >>= as' ("initialialy received " <> frames1 <> " frames")
        delay (Milliseconds 7000.00)                                           >>= as' "let video play again"
        frames2 <- F.getInnerText "#frames" page
        packets2 <- F.getInnerText "#packets" page
        let frameDiff = F.stringToInt frames2 - F.stringToInt frames1
        Assert.assert "packets aren't increasing" ((F.stringToInt packets1) < (F.stringToInt packets2))
                                                                               >>= as' ("packets are increasing: " <> packets2 <> " > " <> packets1)
        Assert.assert "frames aren't increasing" (frameDiff > 65)              >>= as' ("frames increased by: " <> show frameDiff)
        T.close browser
        pure unit

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------
healthNodeToRunState :: JsonLd.HealthNode Array -> RunState
healthNodeToRunState =
  _.runState <<< JsonLd.unwrapNode <<< _.nodeManager <<< JsonLd.unwrapNode

healthNodeToAgentCount :: JsonLd.HealthNode Array -> Int
healthNodeToAgentCount =
  (\{ingests, ingestAggregators, streamRelays, egests} -> ingests + ingestAggregators + streamRelays + egests)
  <<< JsonLd.unwrapNode <<< _.nodeManager <<< JsonLd.unwrapNode
