module Cases.Metrics (metricsTests) where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Debug.Trace (spy)
import Effect.Aff (Aff)
import Helpers.Assert (assertStatusCode, assertAggregator, assertAggregatorOn)
import Helpers.Env as E
import Helpers.Functions (startSession, launch, stopSession, stopNode, maxOut, allNodesBar, aggregatorNotPresent)
import Helpers.HTTP as HTTP
import Helpers.Log (as, as')
import Helpers.Types (Node, ResWithBody)
import Milkis as M
import Shared.Rtsv2.Types (CanaryState(..))
import Test.Spec (SpecT, after_, before_, describe, describeOnly, it, itOnly)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
metricsTests :: forall m. Monad m => SpecT Aff Unit m Unit
metricsTests = do
  describeOnly "10 metrics tests" do
    before_ (startSession nodes *> launch nodes) do
      after_ stopSession do
        ingestMetrics

-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------
nodes :: Array Node
nodes = [E.p1n1, E.p1n2, E.p1n3]


-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------
ingestMetrics :: forall m. Monad m => SpecT Aff Unit m Unit
ingestMetrics =
  it "2.1 ingest aggregation created on ingest node" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.highStreamName
      >>= assertStatusCode 200
      >>= as "create ingest"

    E.waitForAsyncProfileStart >>= as' "wait for async start of profile"

    response <- assertStatusCode 200 =<< HTTP.getIngestMetrics E.p1n1
    shit <- parseBody response
    pure unit
    where
      parseBody :: Either String ResWithBody -> Aff (Either String String)
      parseBody response =
        case response of
          Left e -> pure $ Left e
          Right res -> do
            pure $ Right $ spy "body:" res.body
