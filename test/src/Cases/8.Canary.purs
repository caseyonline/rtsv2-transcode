module Cases.Canary where

import Prelude

import Control.Monad.State (evalStateT, lift)
import Data.Map as Map
import Effect.Aff (Aff, delay, Milliseconds(..))
import Helpers.Types (Node)
import Helpers.Assert as A
import Helpers.CreateString as C
import Helpers.Env as E
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log (as, as', asT)
import Helpers.Log as L
import Helpers.Types (Node)
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Types (CanaryState(..))
import Test.Spec (SpecT, after_, before_, describe, it)
import Toppokki as T

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
canaryTests :: forall m. Monad m => SpecT Aff Unit m Unit
canaryTests =
  describe "8 Canary Tests" do
    canaryWhenLiveTests
    canaryWhenCanaryTests

canaryWhenLiveTests :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenLiveTests = do
  describe "8.1 Canary when Live" do
    before_ (F.startSession nodes *> F.launch nodes) do
      after_ F.stopSession do
        canaryWhenLiveTest1
        canaryWhenLiveTest2
        canaryWhenLiveTest3
        canaryWhenLiveTest4
        canaryWhenLiveTest5
        canaryWhenLiveTest6
        canaryWhenLiveTest7

canaryWhenCanaryTests :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenCanaryTests = do
  describe "8.2 Canary when Canary" do
    before_ (F.startSession nodes *> F.launch' nodes "test/config/canary/sys.config") do
      after_ (F.stopSession *> F.stopSlot) do
        canaryWhenCanary1
        canaryWhenCanary2
        canaryWhenCanary3

-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------
nodes :: Array Node
nodes = [E.p1n1, E.p1n2]

options =
  { headless: false
  , args: E.browserLaunchArgsIng
  , devtools: true
  }

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- In live mode...
canaryWhenLiveTest1 :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenLiveTest1 =
  it "8.1.1 Heathcheck endpoint returns correct canary state" do
    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) Live <<< healthNodeToCanaryState)
                            >>= as "canary state is live"
    pure unit

canaryWhenLiveTest2 :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenLiveTest2 =
  it "8.1.2 Cannot perform RTMP ingest against canary port" do
    F.startSlotHigh1000Canary (C.toAddrFromNode E.p1n1) >>= L.as' "attempt to create high ingest"
    E.waitForIntraPoPDisseminate
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 404 >>= as "no aggregator running"
    pure unit

canaryWhenLiveTest3 :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenLiveTest3 =
  it "8.1.3 Cannot perform WebRTC ingest against canary path" do
    browser <- T.launch options
    page <- T.newPage browser
    T.goto (HTTP.canaryIngestUrl E.p1n1 E.shortName1 E.highStreamName) page
    _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for page to load"
    T.bringToFront page >>= L.as' "Bring to front"

    T.click (T.Selector "#authenticate") page
    _ <- delay (Milliseconds 500.00) >>= L.as' "wait for authentication"

    T.click (T.Selector "#start-ingest") page
    _ <- delay (Milliseconds 2000.00) >>= L.as' "let stream start"

    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 404 >>= as "no aggregator running"
    T.close browser
    pure unit

canaryWhenLiveTest4 :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenLiveTest4 =
  it "8.1.4 Cannot perform egest against canary path" do
    F.startSlotHigh1000 (C.toAddrFromNode E.p1n1) >>= L.as' "create high ingest"
    E.waitForIntraPoPDisseminate
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 200 >>= as "aggregator running"
    HTTP.clientStart E.p1n1 Canary E.slot1 >>= A.assertStatusCode 503 >>= as "egest service not available"
    pure unit

canaryWhenLiveTest5 :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenLiveTest5 =
  it "8.1.5 Cannot transistion to live mode when live" do
    HTTP.changeCanaryState E.p1n1 Live >>= A.assertStatusCode 409 >>= as "conflict returned"
    pure unit

canaryWhenLiveTest6 :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenLiveTest6 =
  it "8.1.6 Cannot transistion to canary mode when there are active agents" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName
                                       >>= A.assertStatusCode 200 >>= as "create low ingest"
    E.waitForAsyncProfileStart
    HTTP.changeCanaryState E.p1n1 Live >>= A.assertStatusCode 409 >>= as "conflict returned"
    HTTP.ingestStop E.p1n1 E.slot1 E.lowProfileName
                                       >>= A.assertStatusCode 200 >>= as "stop low ingest"
    E.waitForMoreThanLinger                                       >>= as' "wait for linger time"
    HTTP.changeCanaryState E.p1n1 Canary >>= A.assertStatusCode 204 >>= as "canary state changed"
    pure unit

canaryWhenLiveTest7 :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenLiveTest7 =
  it "8.1.7 After transistion to canary, can ingest and egest a canary stream" do
    (flip evalStateT) Map.empty $ do
      lift $ HTTP.ingestStart E.p1n1 Canary E.shortName1 E.lowStreamName
                                               >>= A.assertStatusCode 409 >>= as "fail to create low canary ingest"
      lift $ E.waitForIntraPoPDisseminate

      lift $ HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 404 >>= as "aggregator not running"

      lift $ HTTP.changeCanaryState E.p1n1 Canary >>= A.assertStatusCode 204 >>= as "canary state changed"

      lift $ HTTP.ingestStart E.p1n1 Canary E.shortName1 E.lowStreamName
                                               >>= A.assertStatusCode 200 >>= as "create low canary ingest"

      lift $ E.waitForIntraPoPDisseminate

      lift $ HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 200 >>= as "aggregator is running"

      lift $ HTTP.getIntraPoPState E.p1n1 >>= A.assertStatusCode 200 >>= A.assertAggregatorCount E.slot1 1
                                                                     >>= as "aggregator is in intra pop state on p1n1"

      lift $ HTTP.getIntraPoPState E.p1n2 >>= A.assertStatusCode 200 >>= A.assertAggregatorCount E.slot1 0
                                                                     >>= as "aggregator is not in intra pop state on p1n2"

      (lift $ HTTP.clientStart E.p1n1 Canary E.slot1 >>= A.assertStatusCode 204)
                                                     >>= F.storeHeader "x-client-id" "clientId1"
                                                     >>= asT "egest active"

      lift $ E.waitForIntraPoPDisseminate

      lift $ HTTP.changeCanaryState E.p1n1 Live >>= A.assertStatusCode 409 >>= as "attempt to switch to live returns conflict"

      clientId1 <- F.getStateValue "clientId1" "unknown"

      lift $ HTTP.clientStop clientId1  E.p1n1 E.slot1  >>= A.assertStatusCode 204 >>= as "stop client 1 on node1"

      lift $ HTTP.ingestStop E.p1n1 E.slot1 E.lowProfileName
                                                        >>= A.assertStatusCode 200 >>= as "stop low ingest"
      lift $ E.waitForMoreThanLinger                    >>= as' "wait for linger time"

      lift $ HTTP.changeCanaryState E.p1n1 Live >>= A.assertStatusCode 204 >>= as "attempt to switch to live succeeds"

      pure unit

-------------------------------------------------------------------------------
-- In canary mode...
canaryWhenCanary1 :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenCanary1 =
  it "8.2.1 Heathcheck endpoint returns correct canary state" do
    HTTP.healthCheck E.p1n1 >>= A.assertStatusCode 200
                            >>= A.assertBodyFun ((==) Canary <<< healthNodeToCanaryState)
                            >>= as "canary state is canary"
    pure unit

canaryWhenCanary2 :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenCanary2 =
  it "8.2.2 Cannot perform RTMP ingest against live port" do
    F.startSlotHigh1000 (C.toAddrFromNode E.p1n1) >>= L.as' "attempt to create high ingest"
    E.waitForIntraPoPDisseminate
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 404 >>= as "no aggregator running"
    pure unit

canaryWhenCanary3 :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenCanary3 =
  it "8.2.3 Cannot perform WebRTC ingest against live path" do
    browser <- T.launch options
    page <- T.newPage browser
    T.goto (HTTP.ingestUrl E.p1n1 E.shortName1 E.highStreamName) page
    _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for page to load"

    T.click (T.Selector "#authenticate") page
    _ <- delay (Milliseconds 500.00) >>= L.as' "wait for authentication"

    T.click (T.Selector "#start-ingest") page
    _ <- delay (Milliseconds 2000.00) >>= L.as' "let stream start"

    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 404 >>= as "no aggregator running"
    T.close browser
    pure unit

canaryWhenCanary4 :: forall m. Monad m => SpecT Aff Unit m Unit
canaryWhenCanary4 =
  it "8.2.4 Cannot perform egest ingest against live path" do
    F.startSlotHigh1000Canary (C.toAddrFromNode E.p1n1) >>= L.as' "create high ingest"
    E.waitForIntraPoPDisseminate
    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 200 >>= as "aggregator running"
    HTTP.clientStart E.p1n1 Live E.slot1 >>= A.assertStatusCode 503 >>= as "egest service not available"
    pure unit

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------
healthNodeToCanaryState :: JsonLd.HealthNode Array -> CanaryState
healthNodeToCanaryState =
  _.canaryState <<< JsonLd.unwrapNode <<< _.nodeManager <<< JsonLd.unwrapNode
