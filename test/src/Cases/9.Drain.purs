module Cases.Drain where

import Prelude

import Control.Monad.State (evalStateT, lift)
import Data.Map as Map
import Debug.Trace (spy)
import Effect.Aff (Aff, delay, Milliseconds(..))
import Helpers.Types (Node)
import Helpers.Assert as A
import Helpers.CreateString as C
import Helpers.Env as E
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log as L
import Helpers.Log (as, as', asT)
import Shared.Rtsv2.Types (CanaryState(..), RunState(..))
import Shared.Rtsv2.JsonLd as JsonLd
import Test.Spec (SpecT, after_, before_, describe, describeOnly, it, itOnly)
import Toppokki as T

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
drainTests :: forall m. Monad m => SpecT Aff Unit m Unit
drainTests =
  describe "9 Drain Tests" do
    passiveDrainTests

passiveDrainTests :: forall m. Monad m => SpecT Aff Unit m Unit
passiveDrainTests = do
  describe "9.1 Passive Drain" do
    before_ (F.startSession nodes *> F.launch nodes) do
      after_ F.stopSession do
        passiveDrainTest1
        passiveDrainTest2
        passiveDrainTest3

-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------
nodes :: Array Node
nodes = [E.p1n1, E.p1n2]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- passiveDrainTests...
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
                                       >>= (\rwb -> pure $ spy "rwb" rwb)
                                       >>= A.assertStatusCode 200 >>= as "high ingest created"

    E.waitForAsyncProfileStart

    HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 200
                                           >>= A.assertAggregator [E.lowSlotAndProfileName, E.highSlotAndProfileName]
                                                                    >>= as "aggregator still running with both ingests"
    pure unit

passiveDrainTest3 :: forall m. Monad m => SpecT Aff Unit m Unit
passiveDrainTest3 =
  itOnly "9.1.3 Once agents have passively drained, node switches to out-of-service state" do
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


-- tests for invalid transitions - outofservice -> forcedrain, outofservice -> passivedrain
-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------
healthNodeToRunState :: JsonLd.HealthNode -> RunState
healthNodeToRunState =
  _.runState <<< JsonLd.unwrapNode <<< _.nodeManager <<< JsonLd.unwrapNode
