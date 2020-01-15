module Main where

import Prelude

import Affjax (Error, Response)
import Affjax as AX
import Affjax.RequestBody (RequestBody)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut as Json
import Data.Array (sort)
import Data.Either (Either(..), hush)
import Data.Foldable (elem)
import Data.Identity (Identity(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, throwError)
import Effect.Exception (error)
import Foreign (F)
import OsCmd (runProc)
import Shared.Stream (StreamVariantId(..))
import Shared.Types (IngestAggregatorPublicState)
import Simple.JSON (class ReadForeign, E)
import Simple.JSON as SimpleJSON
import Test.Spec (after_, before_, describe, it, itOnly)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT)


type TestNode =  { vlan :: String
                 , addr :: String
                 , sysConfig :: String
                 }

node :: String -> String -> String -> TestNode
node vlan addr sysConfig = {vlan, addr, sysConfig}

main :: Effect Unit
main =
  launchAff_ $ un Identity $ runSpecT testConfig [consoleReporter] do
    before_ (do
                _ <- stopSession
                launchNodes [
                  node "vlan101" "172.16.169.1" "test/config/sys.config"
                  , node "vlan102" "172.16.169.2" "test/config/sys.config"
                  , node "vlan103" "172.16.169.3" "test/config/sys.config"
                  , node "vlan201" "172.16.170.1" "test/config/sys.config"
                  , node "vlan202" "172.16.170.2" "test/config/sys.config"
                  ]) do
      after_ stopSession do
        describe "two node setup" do
          -- it "client requests stream on ingest node" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node1edge = "http://172.16.169.1:3000/api/client/canary/client/slot1/start"
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node1edge
          --   _ <- AX.get ResponseFormat.string node1ingestStart
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1edge
          --   pure unit
          -- it "client requests stream on non-ingest node" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node2edge = "http://172.16.169.2:3000/api/client/canary/client/slot1/start"
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node2edge
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          --   _ <- delay (Milliseconds 1000.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2edge
          --   pure unit
          -- it "client requests stream on other pop" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node2edge = "http://172.16.170.2:3000/api/client/canary/client/slot1/start"
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node2edge
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          --   _ <- delay (Milliseconds 2000.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2edge
          --   pure unit
          -- it "client requests stream on 2nd node on ingest pop" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node2edge = "http://172.16.169.2:3000/api/client/canary/client/slot1/start"
          --     node3edge = "http://172.16.169.3:3000/api/client/canary/client/slot1/start"
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node2edge
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node3edge
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          --   _ <- delay (Milliseconds 2000.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2edge
          --   _ <- delay (Milliseconds 1000.0)
          --   _ <- assertHeader (Tuple "x-servedby" "172.16.169.2") =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node3edge
          --   pure unit
          -- it "client ingest starts and stops" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node1ingestStop = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/stop"
          --     node2edge = "http://172.16.169.2:3000/api/client/canary/client/slot1/start"
          --     node3edge = "http://172.16.170.2:3000/api/client/canary/client/slot1/start"
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node2edge
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node3edge
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          --   _ <- delay (Milliseconds 2000.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2edge
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node3edge
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStop
          --   _ <- delay (Milliseconds 2000.0)
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node2edge
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node3edge
          --   pure unit
          -- it "client edge starts and stops" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node2edgeStart = "http://172.16.169.2:3000/api/client/canary/client/slot1/start"
          --     node2edgeStop = "http://172.16.169.2:3000/api/client/canary/client/slot1/stop"
          --     node3edgeStart = "http://172.16.169.3:3000/api/client/canary/client/slot1/start"
          --     node3edgeStop = "http://172.16.169.3:3000/api/client/canary/client/slot1/stop"
          --     node2edgeCount = "http://172.16.169.2:3000/api/client/canary/edge/slot1/clientCount"
          --     node3edgeCount = "http://172.16.169.3:3000/api/client/canary/edge/slot1/clientCount"
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          --   _ <- delay (Milliseconds 1000.0)
          --   _ <- assertHeader (Tuple "x-servedby" "172.16.169.2") =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node2edgeStart
          --   _ <- delay (Milliseconds 1000.0)
          --   _ <- assertHeader (Tuple "x-servedby" "172.16.169.2") =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node3edgeStart
          --   _ <- assertBody "2" =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node2edgeCount
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node3edgeCount
          --   _ <- assertHeader (Tuple "x-servedby" "172.16.169.2") =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node2edgeStart
          --   _ <- assertHeader (Tuple "x-servedby" "172.16.169.2") =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node3edgeStart
          --   _ <- assertBody "4" =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node2edgeCount
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node3edgeCount
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2edgeStop
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2edgeStop
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2edgeStop
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2edgeStop
          --   _ <- delay (Milliseconds 3000.0) -- alslot1_500 edge linger time to expire...
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node2edgeCount
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node3edgeCount
          --   _ <- delay (Milliseconds 1000.0)
          --   _ <- assertHeader (Tuple "x-servedby" "172.16.169.3") =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node3edgeStart
          --   _ <- assertBody "1" =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node3edgeCount
          --   pure unit
          -- it "ingest aggregation on ingest node" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node1ingestAggregator = "http://172.16.169.1:3000/api/agents/ingestAggregator/slot1"
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestAggregator
          --   pure unit
          -- it "if ingest node is too loaded, then ingest aggregation starts on non-ingest node" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node1ingestLoad = "http://172.16.169.1:3000/api/load"
          --     node3ingestLoad = "http://172.16.169.3:3000/api/load"
          --     node1ingestAggregator = "http://172.16.169.1:3000/api/agents/ingestAggregator/slot1"
          --     node2ingestAggregator = "http://172.16.169.2:3000/api/agents/ingestAggregator/slot1"
          --     node3ingestAggregator = "http://172.16.169.3:3000/api/agents/ingestAggregator/slot1"
          --     assertAggregators :: E IngestAggregatorPublicState -> Boolean
          --     assertAggregators (Left _) = false
          --     assertAggregators (Right {activeStreamVariants}) = [StreamVariantId "slot1" "slot1_500"] == (sort $ _.streamVariant <$> activeStreamVariants)
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node1ingestLoad (jsonBody "{\"load\": 60.0}")
          --   _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node3ingestLoad (jsonBody "{\"load\": 60.0}")
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node1ingestAggregator
          --   _ <- assertBodyFun assertAggregators =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node2ingestAggregator
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node3ingestAggregator
          --   pure unit
          -- it "2nd ingest doesn't start new aggregator since one is running" do
          --   let
          --     node1ingestStart1 = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node1ingestStart2 = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_1000/start"
          --     node1ingestAggregator = "http://172.16.169.1:3000/api/agents/ingestAggregator/slot1"
          --     node1ingestLoad = "http://172.16.169.1:3000/api/load"
          --     assertAggregators :: E IngestAggregatorPublicState -> Boolean
          --     assertAggregators (Left _) = false
          --     assertAggregators (Right {activeStreamVariants}) = [StreamVariantId "slot1" "slot1_1000", StreamVariantId "slot1" "slot1_500"] == (sort $ _.streamVariant <$> activeStreamVariants)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart1
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node1ingestLoad (jsonBody "{\"load\": 60.0}")
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart2
          --   _ <- assertBodyFun assertAggregators =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestAggregator
          --   _ <- delay (Milliseconds 1000.0)
          --   pure unit
          -- it "ingest restarts aggregator if aggregator exits" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node1ingestLoad = "http://172.16.169.1:3000/api/load"
          --     node3ingestLoad = "http://172.16.169.3:3000/api/load"
          --     node1ingestAggregator = "http://172.16.169.1:3000/api/agents/ingestAggregator/slot1"
          --     node2ingestAggregator = "http://172.16.169.2:3000/api/agents/ingestAggregator/slot1"
          --     node3ingestAggregator = "http://172.16.169.3:3000/api/agents/ingestAggregator/slot1"
          --     assertAggregators :: E IngestAggregatorPublicState -> Boolean
          --     assertAggregators (Left _) = false
          --     assertAggregators (Right {activeStreamVariants}) = [StreamVariantId "slot1" "slot1_500"] == (sort $ _.streamVariant <$> activeStreamVariants)
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node1ingestLoad (jsonBody "{\"load\": 60.0}")
          --   _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node3ingestLoad (jsonBody "{\"load\": 50.0}")
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2ingestAggregator
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node3ingestAggregator
          --   _ <- stopNode "172.16.169.2"
          --   _ <- delay (Milliseconds 2500.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node3ingestAggregator
          --   pure unit
          -- it "aggregator exits after last variant stops (with linger time)" do
          --   let
          --     node1ingestStart1 = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node1ingestStart2 = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_1000/start"
          --     node1ingestStop1 = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/stop"
          --     node1ingestStop2 = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_1000/stop"
          --     node1ingestAggregator = "http://172.16.169.1:3000/api/agents/ingestAggregator/slot1"
          --     assertBothVariants :: E IngestAggregatorPublicState -> Boolean
          --     assertBothVariants (Left _) = false
          --     assertBothVariants (Right {activeStreamVariants}) = [StreamVariantId "slot1" "slot1_1000", StreamVariantId "slot1" "slot1_500"] == (sort $ _.streamVariant <$> activeStreamVariants)
          --     assertOneVariant :: E IngestAggregatorPublicState -> Boolean
          --     assertOneVariant (Left _) = false
          --     assertOneVariant (Right {activeStreamVariants}) = [StreamVariantId "slot1" "slot1_1000"] == (_.streamVariant <$> activeStreamVariants)
          --     assertNoVariant :: E IngestAggregatorPublicState -> Boolean
          --     assertNoVariant (Left _) = false
          --     assertNoVariant (Right {activeStreamVariants}) = [] == activeStreamVariants
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart1
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart2
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertBodyFun assertBothVariants =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestAggregator
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStop1
          --   _ <- assertBodyFun assertOneVariant =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestAggregator
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStop2
          --   _ <- assertBodyFun assertNoVariant =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestAggregator
          --   _ <- delay (Milliseconds 1500.0)
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node1ingestAggregator
          --   pure unit
          -- it "aggregator lingers exits after last variant stops" do
          --   let
          --     node1ingestStart1 = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
          --     node2ingestStart2 = "http://172.16.169.2:3000/api/client/:canary/ingest/mmddev001/slot1_1000/start"
          --     node1ingestStop1 = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/stop"
          --     node2ingestStop2 = "http://172.16.169.2:3000/api/client/:canary/ingest/mmddev001/slot1_1000/stop"
          --     node1ingestAggregator = "http://172.16.169.1:3000/api/agents/ingestAggregator/slot1"
          --     assertOneVariant :: E IngestAggregatorPublicState -> Boolean
          --     assertOneVariant (Left _) = false
          --     assertOneVariant (Right {activeStreamVariants}) = [StreamVariantId "slot1" "slot1_1000"] == (_.streamVariant <$> activeStreamVariants)
          --   _ <- delay (Milliseconds 500.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart1  -- start on node 1
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestAggregator -- check aggregator is on node 1
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStop1 -- stop on node 1
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestAggregator -- check aggregator is still running
          --   _ <- delay (Milliseconds 500.0) -- sleep for a bit (but less than linger time)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2ingestStart2 -- start on node2
          --   _ <- assertBodyFun assertOneVariant =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestAggregator -- check aggregator is still on node 1 and has the ingest from node 2
          --   _ <- delay (Milliseconds 500.0)
          --   pure unit
          it "ingest on different node removes itself from aggregator when stopped" do
            let
              node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
              node1ingestStop = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/stop"
              node1ingestLoad = "http://172.16.169.1:3000/api/load"
              node3ingestLoad = "http://172.16.169.3:3000/api/load"
              node1ingestAggregator = "http://172.16.169.1:3000/api/agents/ingestAggregator/slot1"
              node2ingestAggregator = "http://172.16.169.2:3000/api/agents/ingestAggregator/slot1"
              node3ingestAggregator = "http://172.16.169.3:3000/api/agents/ingestAggregator/slot1"
              hasOneVariant :: E IngestAggregatorPublicState -> Boolean
              hasOneVariant (Left _) = false
              hasOneVariant (Right {activeStreamVariants}) = [StreamVariantId "slot1" "slot1_500"] == (sort $ _.streamVariant <$> activeStreamVariants)
              hasNoVariants :: E IngestAggregatorPublicState -> Boolean
              hasNoVariants (Left _) = false
              hasNoVariants (Right {activeStreamVariants}) = [] == activeStreamVariants
            _ <- delay (Milliseconds 500.0)
            _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node1ingestLoad (jsonBody "{\"load\": 60.0}")
            _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node3ingestLoad (jsonBody "{\"load\": 50.0}")
            _ <- delay (Milliseconds 500.0)
            _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart -- Aggregator will start on 2
            _ <- delay (Milliseconds 500.0)
            _ <- assertBodyFun hasOneVariant =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node2ingestAggregator
            _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStop
            _ <- delay (Milliseconds 500.0)
            _ <- assertBodyFun hasNoVariants =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node2ingestAggregator
            pure unit

  where
    testConfig = { slow: Milliseconds 5000.0, timeout: Just (Milliseconds 25000.0), exit: false }

assertStatusCode :: forall a. Int -> Either Error (Response a) -> Aff (Response a)
assertStatusCode expectedCode either =
  case spy "either" either of
    Right response@{status : StatusCode resultCode} | resultCode == expectedCode ->
      pure response
    Right response@{status : StatusCode resultCode} ->
      throwError $ error $ "Unexpected statuscode " <> show response.status <> ", expected " <> show expectedCode
    Left err ->
      throwError $ error $ "GET /api response failed to decode: " <> AX.printError err

assertHeader :: forall a. Tuple String String -> Response a -> Aff (Response a)
assertHeader (Tuple header value) response@{headers} =
  case elem (ResponseHeader header value) headers of
    true -> pure response
    false -> throwError $ error $ "Header " <> header <> ":" <> value <> " not present in response " <> (show headers)

assertBodyFun :: forall a. ReadForeign a => (E a -> Boolean) -> Response String -> Aff (Response String)
assertBodyFun expectedBodyFun response@{body} =
  let
    parsed = SimpleJSON.readJSON body
  in
   if expectedBodyFun parsed then pure response
   else throwError $ error $ "Body " <> body <> " did not match expected"

assertBody :: String -> Response String -> Aff (Response String)
assertBody expectedBody response@{body} =
  if expectedBody == body then pure response
  else throwError $ error $ "Body " <> body <> " did not match expected " <> expectedBody

jsonBody :: String -> Maybe RequestBody
jsonBody string =
  RequestBody.json <$> hush (Json.jsonParser string)

sessionName:: String
sessionName = "testSession"

launchNodes :: Array TestNode -> Aff Unit
launchNodes sysconfigs = do
  _ <- runProc "./scripts/startSession.sh" [sessionName]
  _ <- traverse_ (\tn -> runProc "./scripts/startNode.sh"
                         [ sessionName
                         , tn.addr
                         , tn.vlan
                         , tn.addr
                         , tn.sysConfig
                         ]) sysconfigs

  traverse_ (\tn -> runProc "./scripts/waitForNode.sh"
                    [ tn.addr
                    ]) sysconfigs

stopSession :: Aff Unit
stopSession = do
  runProc "./scripts/stopSession.sh" [sessionName]

stopNode :: String -> Aff Unit
stopNode nodeAddr = do
  runProc "./scripts/stopNode.sh" [ sessionName
                                  , nodeAddr
                                  , nodeAddr
                                  ]

