module Main where

import Prelude

import Affjax (Error, Response)
import Affjax as AX
import Affjax.RequestBody (RequestBody)
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut as Json
import Data.Argonaut.Core as Data.Argonaut.Core
import Data.Either (Either(..), hush)
import Data.Foldable (elem)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, throwError)
import Effect.Exception (Error, error) as Exception
import OsCmd (runProc)
import Simple.JSON (class ReadForeign, E)
import Simple.JSON as SimpleJSON
import Test.Spec (after_, before_, describe, it)
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
  let
    p1n1                         = "http://172.16.169.1:3000/api/"
    p1n2                         = "http://172.16.169.2:3000/api/"
    p2n1                         = "http://172.16.170.1:3000/api/"
    p2n2                         = "http://172.16.170.2:3000/api/"

    egest  node' streamId         = AX.put ResponseFormat.string (node' <> "client/canary/client/" <> streamId <> "/start") (Just $ Affjax.RequestBody.json $ Data.Argonaut.Core.fromString "{}")
    ingest node' streamId variant = AX.get ResponseFormat.string (node' <> "client/canary/ingest/" <> streamId <> "/" <> variant <> "/start")
    relayStatus node' streamId    = AX.get ResponseFormat.string (node' <> "relay/" <> streamId)

    stream1                       = "stream1"
    low                           = "low"
    delayMs                       = delay <<< Milliseconds
  in
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
        describe "Ingest egest" do
          describe "one pop setup" do
            it "client requests stream on ingest node" do
              _ <- egest       p1n1 stream1     >>= assertStatusCode 404 "no egest prior to ingest"
              _ <- relayStatus p1n1 stream1     >>= assertStatusCode 404 "no relay prior to ingest"
              _ <- ingest      p1n1 stream1 low >>= assertStatusCode 200 "create ingest"
              _ <- delayMs 500.0
              _ <- egest       p1n1 stream1     >>= assertStatusCode 204 "egest available"
              _ <- relayStatus p1n1 stream1     >>= assertStatusCode 200 "relay exists"
              pure unit

            it "client requests stream on non-ingest node" do
              _ <- egest       p1n2 stream1     >>= assertStatusCode 404 "no egest prior to ingest"
              _ <- relayStatus p1n2 stream1     >>= assertStatusCode 404 "no remote relay prior to ingest"
              _ <- ingest      p1n1 stream1 low >>= assertStatusCode 200 "create ingest"
              _ <- delayMs 1000.0
              _ <- egest       p1n2 stream1     >>= assertStatusCode 204 "egest available"
              _ <- relayStatus p1n2 stream1     >>= assertStatusCode 200 "remote relay exists"
              pure unit

          describe "two pop setup" do
            it "client requests stream on other pop" do
              _ <- egest       p2n2 stream1     >>= assertStatusCode 404 "no egest prior to ingest"
              _ <- relayStatus p1n1 stream1     >>= assertStatusCode 404 "no remote relay prior to ingest"
              _ <- relayStatus p1n1 stream1     >>= assertStatusCode 404 "no local relay prior to ingest"
              _ <- ingest      p1n1 stream1 low >>= assertStatusCode 200 "create ingest"
              _ <- delayMs 1000.0
              _ <- egest       p2n2 stream1     >>= assertStatusCode 204 "egest available"
              _ <- relayStatus p2n2 stream1     >>= assertStatusCode 200 "local relay exists"
              _ <- relayStatus p1n1 stream1     >>= assertStatusCode 200 "remote relay exists"
              pure unit

          -- it "client requests stream on other pop" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/low/start"
          --     node2edge = "http://172.16.170.2:3000/api/client/canary/client/stream1/start"
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node2edge
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          --   _ <- delay (Milliseconds 2000.0)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2edge
          --   pure unit
          -- it "client requests stream on 2nd node on ingest pop" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/low/start"
          --     node2edge = "http://172.16.169.2:3000/api/client/canary/client/stream1/start"
          --     node3edge = "http://172.16.169.3:3000/api/client/canary/client/stream1/start"
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
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/low/start"
          --     node1ingestStop = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/low/stop"
          --     node2edge = "http://172.16.169.2:3000/api/client/canary/client/stream1/start"
          --     node3edge = "http://172.16.170.2:3000/api/client/canary/client/stream1/start"
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
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/low/start"
          --     node2edgeStart = "http://172.16.169.2:3000/api/client/canary/client/stream1/start"
          --     node2edgeStop = "http://172.16.169.2:3000/api/client/canary/client/stream1/stop"
          --     node3edgeStart = "http://172.16.169.3:3000/api/client/canary/client/stream1/start"
          --     node3edgeStop = "http://172.16.169.3:3000/api/client/canary/client/stream1/stop"
          --     node2edgeCount = "http://172.16.169.2:3000/api/egest/stream1/clientCount"
          --     node3edgeCount = "http://172.16.169.3:3000/api/egest/stream1/clientCount"
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
          --   _ <- delay (Milliseconds 3000.0) -- allow edge linger time to expire...
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node2edgeCount
          --   _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node3edgeCount
          --   _ <- delay (Milliseconds 1000.0)
          --   _ <- assertHeader (Tuple "x-servedby" "172.16.169.3") =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node3edgeStart
          --   _ <- assertBody "1" =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node3edgeCount
          --   pure unit
          -- it "ingest aggregation on ingest node" do
          --   let
          --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/low/start"
          --     node1ingestAggregator = "http://172.16.169.1:3000/api/agents/ingestAggregator/stream1"
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestAggregator
          --   pure unit
          -- -- it "ingest aggregation on non-ingest node" do
          -- --   let
          -- --     node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/low/start"
          -- --     node1ingestLoad = "http://172.16.169.1:3000/api/load"
          -- --   _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node1ingestLoad SomeLoadJsonHere
          -- --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
          -- -- TODO - assert that ingest aggregator is on node2 (or 3) - using new /api/agents/ingestAggregator endpoint
          -- it "2nd ingest aggregation on ingest node" do
          --   let
          --     node1ingestStart1 = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/low/start"
          --     node1ingestStart2 = "http://172.16.169.1:3000/api/client/:canary/ingest/stream1/high/start"
          --     node1ingestAggregator = "http://172.16.169.1:3000/api/agents/ingestAggregator/stream1"
          --     node1ingestLoad = "http://172.16.169.1:3000/api/load"
          --     assertAggregators :: E (Array StreamAndVariant) -> Boolean
          --     assertAggregators (Left _) = false
          --     assertAggregators (Right streamVariants) = [StreamAndVariant (StreamId "stream1") (StreamVariant "high"), StreamAndVariant (StreamId "stream1") (StreamVariant "low")] == (sort streamVariants)
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart1
          --   _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node1ingestLoad (jsonBody "{\"load\": 60.0}")
          --   _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart2
          --   _ <- assertBodyFun assertAggregators =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestAggregator
          --   _ <- delay (Milliseconds 1000.0)
          --   pure unit

  where
    testConfig = { slow: Milliseconds 5000.0, timeout: Just (Milliseconds 20000.0), exit: false }

assertStatusCode :: forall a. Int -> String -> Either Error (Response a) -> Aff (Response a)
assertStatusCode expectedCode source either =
  case either of
    Right response@{status : StatusCode resultCode} | resultCode == expectedCode ->
      pure response
    Right response@{status : StatusCode resultCode} ->
      throwSlowError $ Exception.error $ "Unexpected statuscode from " <> source <> " - " <> show response.status
    Left err ->
      throwSlowError $ Exception.error $ "GET /api response failed to decode from " <> source <> " - " <> AX.printError err

assertHeader :: forall a. Tuple String String -> Response a -> Aff (Response a)
assertHeader (Tuple header value) response@{headers} =
  case elem (ResponseHeader header value) headers of
    true -> pure response
    false -> throwSlowError $ Exception.error $ "Header " <> header <> ":" <> value <> " not present in response " <> (show headers)

assertBodyFun :: forall a. ReadForeign a => (E a -> Boolean) -> Response String -> Aff (Response String)
assertBodyFun expectedBodyFun response@{body} =
  let
    parsed = SimpleJSON.readJSON body
  in
   if expectedBodyFun parsed then pure response
   else throwSlowError $ Exception.error $ "Body " <> body <> " did not match expected"

assertBody :: String -> Response String -> Aff (Response String)
assertBody expectedBody response@{body} =
  if expectedBody == body then pure response
  else throwSlowError $ Exception.error $ "Body " <> body <> " did not match expected " <> expectedBody

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



throwSlowError :: forall e. Exception.Error -> Aff e
throwSlowError e =
  do
    _ <- delay (Milliseconds 1000.0)
    throwError e
