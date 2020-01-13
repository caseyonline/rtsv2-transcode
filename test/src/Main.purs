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

data Node = Node Int Int

main :: Effect Unit
main =
  let

    p1n1                         = Node 1 1
    p1n2                         = Node 1 2
    p1n3                         = Node 1 3
    p2n1                         = Node 2 1
    p2n2                         = Node 2 2

    stream1                       = "stream1"
    low                           = "low"

    toAddr (Node popNum nodeNum) = "172.16." <> show (popNum + 168) <> "." <> show nodeNum
    toVlan (Node popNum nodeNum) = "vlan" <> show (popNum * 10) <> show nodeNum

    api node = "http://" <> toAddr node <> ":3000/api/"

    stringifyError (Right r)       = Right r
    stringifyError (Left axError)  = Left $ AX.printError axError

    egest  node streamId         = AX.put ResponseFormat.string (api node <> "client/canary/client/" <> streamId <> "/start") (Just $ Affjax.RequestBody.json $ Data.Argonaut.Core.fromString "{}") <#> stringifyError
    ingest node streamId variant = AX.get ResponseFormat.string (api node <> "client/canary/ingest/" <> streamId <> "/" <> variant <> "/start") <#> stringifyError
    relayStatus node streamId    = AX.get ResponseFormat.string (api node <> "relay/" <> streamId) <#> stringifyError

    mkNode sysConfig node = {vlan: toVlan node, addr: toAddr node, sysConfig}


    delayMs                       = delay <<< Milliseconds

    as desc (Right _) = pure unit
    as desc (Left err) = throwSlowError $ "Step: \"" <> desc <> "\" failed with reason: " <>err

    start nodes = do
      _ <- stopSession
      nodes <#> mkNode "test/config/sys.config" # launchNodes

  in
  launchAff_ $ un Identity $ runSpecT testConfig [consoleReporter] do
    describe "Ingest egest tests" do
      before_ (start [p1n1, p1n2, p1n3]) do
        after_ stopSession do
          describe "one pop setup" do
            it "client requests stream on ingest node" do
              egest       p1n1 stream1     >>= assertStatusCode 404 >>= as "no egest prior to ingest"
              egest       p1n1 stream1     >>= assertStatusCode 404 >>= as "no egest prior to ingest"
              relayStatus p1n1 stream1     >>= assertStatusCode 404 >>= as "no relay prior to ingest"
              ingest      p1n1 stream1 low >>= assertStatusCode 200 >>= as "create ingest"
              delayMs 500.0
              egest       p1n1 stream1     >>= assertStatusCode 204 >>= as "egest available"
              relayStatus p1n1 stream1     >>= assertStatusCode 200 >>= as "relay exists"

            it "client requests stream on non-ingest node" do
              egest       p1n2 stream1     >>= assertStatusCode 404 >>= as "no egest prior to ingest"
              relayStatus p1n2 stream1     >>= assertStatusCode 404 >>= as "no remote relay prior to ingest"
              ingest      p1n1 stream1 low >>= assertStatusCode 200 >>= as "create ingest"
              delayMs 1000.0
              egest       p1n2 stream1     >>= assertStatusCode 204 >>= as "egest available"
              relayStatus p1n2 stream1     >>= assertStatusCode 200 >>= as "remote relay exists"

            it "client requests stream on 2nd node on ingest pop" do
              egest       p1n2 stream1     >>= assertStatusCode 404 >>= as "no egest p1n2 prior to ingest"
              egest       p1n2 stream1     >>= assertStatusCode 404 >>= as "no egest p1n3 prior to ingest"
              ingest      p1n1 stream1 low >>= assertStatusCode 200 >>= as "create ingest"
              delayMs 1000.0
              egest       p1n2 stream1     >>= assertStatusCode 204 >>= as "egest available on p1n2"
              delayMs 1000.0
              egest       p1n3 stream1     >>= assertStatusCode 204
                                           >>= assertHeader (Tuple "x-servedby" "172.16.169.2")
                                                                    >>= as "p1n3 egest redirects to p1n2"

      describe "two pop setup" do
        before_ (start [p1n1, p1n2, p1n3, p2n1]) do
          after_ stopSession do
            it "client requests stream on other pop" do
              egest       p2n1 stream1     >>= assertStatusCode 404 >>= as "no egest prior to ingest"
              relayStatus p1n1 stream1     >>= assertStatusCode 404 >>= as "no remote relay prior to ingest"
              relayStatus p1n1 stream1     >>= assertStatusCode 404 >>= as "no local relay prior to ingest"
              ingest      p1n1 stream1 low >>= assertStatusCode 200 >>= as "create ingest"
              delayMs 1000.0
              egest       p2n1 stream1     >>= assertStatusCode 204 >>= as "egest available"
              relayStatus p2n1 stream1     >>= assertStatusCode 200 >>= as "local relay exists"
              -- TODO -- relayStatus p1n1 stream1     >>= assertStatusCode 200 >>= as "remote relay exists"

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

assertStatusCode :: forall a. Int -> Either String (Response a) -> Aff (Either String (Response a))
assertStatusCode expectedCode either =
  pure $ either >>= (\response@{status : StatusCode resultCode} ->
                      if resultCode == expectedCode
                      then pure response
                      else Left $  "Unexpected statuscode: Expected " <> show expectedCode <> ", got " <> show resultCode
                    )

assertHeader :: forall a. Tuple String String -> Either String (Response a) -> Aff (Either String (Response a))
assertHeader (Tuple header value) either =
  pure $ either >>= (\response@{headers} ->
                      if elem (ResponseHeader header value) headers
                      then pure response
                      else Left $ "Header " <> header <> ":" <> value <> " not present in response " <> (show headers)
                    )
-- assertBodyFun :: forall a. ReadForeign a => (E a -> Boolean) -> Response String -> Aff (Response String)
-- assertBodyFun expectedBodyFun response@{body} =
--   let
--     parsed = SimpleJSON.readJSON body
--   in
--    if expectedBodyFun parsed then pure response
--    else throwSlowError $ Exception.error $ "Body " <> body <> " did not match expected"

-- assertBody :: String -> Response String -> Aff (Response String)
-- assertBody expectedBody response@{body} =
--   if expectedBody == body then pure response
--   else throwSlowError $ Exception.error $ "Body " <> body <> " did not match expected " <> expectedBody

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



throwSlowError :: forall e. String -> Aff e
throwSlowError e =
  do
    _ <- delay (Milliseconds 200.0)
    throwError $ Exception.error e
