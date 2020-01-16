module Main where

import Prelude

import Data.Array (delete, sort, sortWith)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, launchAff_, throwError)
import Effect.Exception (error) as Exception
import Foreign.Object as Object
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import OsCmd (runProc)
import Shared.Stream (StreamVariant(..))
import Shared.Types (IngestAggregatorPublicState)
import Simple.JSON (class ReadForeign)
import Simple.JSON as SimpleJSON
import Test.Spec (after_, before_, describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT)

fetch :: M.Fetch
fetch = M.fetch nodeFetch

type TestNode =  { vlan :: String
                 , addr :: String
                 , sysConfig :: String
                 }

data Node = Node Int Int
derive instance eqNode :: Eq Node

main :: Effect Unit
main =
  let

    p1n1 = Node 1 1
    p1n2 = Node 1 2
    p1n3 = Node 1 3
    p2n1 = Node 2 1
    p2n2 = Node 2 2

    slot1      = "slot1"
    shortName1 = "mmddev001"
    low        = "slot1_500"
    high       = "slot1_1000"

    start = "start"
    stop  = "stop"

    toAddr (Node popNum nodeNum) = "172.16." <> show (popNum + 168) <> "." <> show nodeNum
    toVlan (Node popNum nodeNum) = "vlan" <> show (popNum * 10) <> show nodeNum
    mkNode sysConfig node = {vlan: toVlan node, addr: toAddr node, sysConfig}

    api node = "http://" <> toAddr node <> ":3000/api/"

    stringifyError (Right r)      = Right r
    stringifyError (Left axError) = Left $ show axError

    client verb node streamId           = fetch (M.URL $ api node <> "public/canary/client/" <> streamId <> "/" <> verb)
                                         { method: M.postMethod
                                         , body: "{}"
                                         , headers: M.makeHeaders { "Content-Type": "application/json" }
                                         } # attempt <#> stringifyError
    egestStats node streamId           = fetch (M.URL $ api node <> "agents/egest/" <> streamId)
                                         { method: M.getMethod
                                         } # attempt <#> stringifyError

    aggregatorStats node streamId      = fetch (M.URL $ api node <> "agents/ingestAggregator/" <> streamId)
                                         { method: M.getMethod
                                         } # attempt <#> stringifyError

    ingest verb node shortName variant = fetch (M.URL $ api node <> "public/canary/ingest/" <> shortName <> "/" <> variant <> "/" <> verb)
                                         { method: M.getMethod
                                         } # attempt <#> stringifyError

    relayStats node streamId           = fetch (M.URL $ api node <> "agents/relay/" <> streamId)
                                         { method: M.getMethod
                                         } # attempt <#> stringifyError

    setLoad :: Node -> Number -> Aff (Either String M.Response)
    setLoad node load                  = fetch (M.URL $ api node <> "load")
                                         { method: M.postMethod
                                         , body: "{\"load\": " <> show load <> "}"
                                         , headers: M.makeHeaders { "Content-Type": "application/json" }
                                         } # attempt <#> stringifyError



    delayMs = delay <<< Milliseconds

    as desc (Right _) =
      let _ = spy "step" desc in
      pure unit
    as desc (Left err) = throwSlowError $ "Step: \"" <> desc <> "\" failed with reason: " <>err

    debug either = let _ = spy "debug" either in either

    debugBody (Right r) = do
      text <- M.text r
      let _ = spy "debugBody" text
      pure $ Right r
    debugBody (Left err) = let _ = spy "debugBodyErr" err in pure $ Left err

    launch nodes = do
      _ <- stopSession
      nodes <#> mkNode "test/config/sys.config" # launchNodes

    assertAggregator variants = assertBodyFun $ predicate variants
      where
        predicate :: Array String -> IngestAggregatorPublicState -> Boolean
        predicate vars {activeStreamVariants} = sort (StreamVariant <$> vars) == (sort $ _.streamVariant <$> activeStreamVariants)

  in
  launchAff_ $ un Identity $ runSpecT testConfig [consoleReporter] do
    describe "Ingest tests"
      let
        nodes = [p1n1, p1n2, p1n3]
        allNodesBar node = delete node nodes
        maxOut server = setLoad server 60.0 >>= assertStatusCode 204 >>= as ("set load on " <> toAddr server)
      in do
      before_ (launch nodes) do
        after_ stopSession do
          it "ingest aggregation created on ingest node" do
            ingest start    p1n1 shortName1 low >>= assertStatusCode 200 >>= as "create ingest"
            delayMs 10.0
            aggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as "aggregator has low only"


          it "2nd ingest does not doesn't start new aggregator since one is running" do
            ingest start    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "create low ingest"
            setLoad         p1n1 60.0            >>= assertStatusCode 204 >>= as "set load on server"
            ingest start    p1n1 shortName1 high >>= assertStatusCode 200 >>= as "create high ingest"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as "aggregator has 2 variants"

          it "if ingest node is too loaded, then ingest aggregation starts on non-ingest node" do
            traverse_ maxOut $ allNodesBar p1n2
            delayMs 1000.0
            ingest start    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "create low ingest"
            delayMs 1000.0
            ingest start    p1n1 shortName1 high >>= assertStatusCode 200 >>= as "create high ingest"
            aggregatorStats p1n2 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as "aggregator is on p1n2"


          it "ingest on different node removes itself from aggregator when stopped" do
            traverse_ maxOut $ allNodesBar p1n2
            delayMs 1000.0
            ingest start    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "create low ingest"
            delayMs 100.0
            aggregatorStats p1n2 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as "aggregator created on idle server"
            ingest stop     p1n1 shortName1 low >>= assertStatusCode 200  >>= as "stop low ingest"
            delayMs 100.0
            aggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                          >>= as "aggregator has no variants"

      --     it "ingest restarts aggregator if aggregator exits" do
      --       let
      --         node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
      --         node1ingestLoad = "http://172.16.169.1:3000/api/load"
      --         node3ingestLoad = "http://172.16.169.3:3000/api/load"
      --         node1ingestAggregator = "http://172.16.169.1:3000/api/agents/ingestAggregator/slot1"
      --         node2ingestAggregator = "http://172.16.169.2:3000/api/agents/ingestAggregator/slot1"
      --         node3ingestAggregator = "http://172.16.169.3:3000/api/agents/ingestAggregator/slot1"
      --         assertAggregators :: E IngestAggregatorPublicState -> Boolean
      --         assertAggregators (Left _) = false
      --         assertAggregators (Right {activeStreamVariants}) = [StreamVariantId "slot1" "slot1_500"] == (sort $ _.streamVariant <$> activeStreamVariants)
      --       _ <- delay (Milliseconds 500.0)
      --       _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node1ingestLoad (jsonBody "{\"load\": 60.0}")
      --       _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node3ingestLoad (jsonBody "{\"load\": 50.0}")
      --       _ <- delay (Milliseconds 500.0)
      --       _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart
      --       _ <- delay (Milliseconds 500.0)
      --       _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node2ingestAggregator
      --       _ <- assertStatusCode 404 =<< AX.get ResponseFormat.string node3ingestAggregator
      --       _ <- stopNode "172.16.169.2"
      --       _ <- delay (Milliseconds 2500.0)
      --       _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node3ingestAggregator
      --       pure unit
      --       pure unit





      --     it
      --       let
      --         node1ingestStart = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/start"
      --         node1ingestStop = "http://172.16.169.1:3000/api/client/:canary/ingest/mmddev001/slot1_500/stop"
      --         node1ingestLoad = "http://172.16.169.1:3000/api/load"
      --         node3ingestLoad = "http://172.16.169.3:3000/api/load"
      --         node1ingestAggregator = "http://172.16.169.1:3000/api/agents/ingestAggregator/slot1"
      --         node2ingestAggregator = "http://172.16.169.2:3000/api/agents/ingestAggregator/slot1"
      --         node3ingestAggregator = "http://172.16.169.3:3000/api/agents/ingestAggregator/slot1"
      --         hasOneVariant :: E IngestAggregatorPublicState -> Boolean
      --         hasOneVariant (Left _) = false
      --         hasOneVariant (Right {activeStreamVariants}) = [StreamVariantId "slot1" "slot1_500"] == (sort $ _.streamVariant <$> activeStreamVariants)
      --         hasNoVariants :: E IngestAggregatorPublicState -> Boolean
      --         hasNoVariants (Left _) = false
      --         hasNoVariants (Right {activeStreamVariants}) = [] == activeStreamVariants
      --       _ <- delay (Milliseconds 500.0)
      --       _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node1ingestLoad (jsonBody "{\"load\": 60.0}")
      --       _ <- assertStatusCode 204 =<< AX.post ResponseFormat.string node3ingestLoad (jsonBody "{\"load\": 50.0}")
      --       _ <- delay (Milliseconds 500.0)
      --       _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStart -- Aggregator will start on 2
      --       _ <- delay (Milliseconds 500.0)
      --       _ <- assertBodyFun hasOneVariant =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node2ingestAggregator
      --       _ <- assertStatusCode 200 =<< AX.get ResponseFormat.string node1ingestStop
      --       _ <- delay (Milliseconds 500.0)
      --       _ <- assertBodyFun hasNoVariants =<< assertStatusCode 200 =<< AX.get ResponseFormat.string node2ingestAggregator
      --       pure unit


          -- it "aggregator exits after last variant stops (with linger time)" do
          --   ingest start    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "create low ingest"
          --   ingest start    p1n1 shortName1 high >>= assertStatusCode 200 >>= as "create high ingest"
          --   aggregatorStats p1n1 slot1           >>= assertStatusCode 200
          --                                            >>= assertAggregator [low, high]
          --                                                                 >>= as "aggregator has both variants"

          --   ingest stop     p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "stop low ingest"
          --   aggregatorStats p1n1 slot1           >>= assertStatusCode 200
          --                                            >>= assertAggregator [high]
          --                                                                 >>= as "aggregator only has high"
          --   ingest stop     p1n1 shortName1 high >>= assertStatusCode 200 >>= as "stop high ingest"
          --   aggregatorStats p1n1 slot1           >>= assertStatusCode 200
          --                                            >>= assertAggregator []
          --                                                                 >>= as "aggregator has no variants"
          --   delayMs 1500.0
          --   aggregatorStats p1n1 slot1           >>= assertStatusCode 404 >>= as "aggregator stops after linger"

          -- it "aggregator does not exit during linger time" do
          --   ingest start    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "create low ingest"
          --   aggregatorStats p1n1 slot1           >>= assertStatusCode 200
          --                                            >>= assertAggregator [low]
          --                                                                 >>= as "aggregator created"
          --   ingest stop     p1n1 shortName1 low >>= assertStatusCode 200  >>= as "stop low ingest"
          --   aggregatorStats p1n1 slot1          >>= assertStatusCode 200
          --                                           >>= assertAggregator []
          --                                                                 >>= as "aggregator has no variants"
          --   delayMs 500.0 -- sleep for a bit (but less than linger time)
          --   ingest start    p1n2 shortName1 high >>= assertStatusCode 200 >>= as "create high ingest on another node"
          --   aggregatorStats p1n1 slot1           >>= assertStatusCode 200
          --                                            >>= assertAggregator [high]
          --                                                                 >>= as "lingered aggregator has high variant"


    -- describe "Ingest egest tests" do
    --   describe "one pop setup"
    --     let
    --       nodes = [p1n1, p1n2, p1n3]
    --       allNodesBar node = delete node nodes
    --     in do
    --     before_ (launch nodes) do
    --       after_ stopSession do
    --         it "client requests stream on ingest node" do
    --           client start p1n1 slot1          >>= assertStatusCode 404 >>= as "no egest prior to ingest"
    --           relayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as "no relay prior to ingest"
    --           ingest start p1n1 shortName1 low >>= assertStatusCode 200 >>= as "create ingest"
    --           delayMs 500.0
    --           client start p1n1 slot1          >>= assertStatusCode 204 >>= as "egest available"
    --           relayStats   p1n1 slot1          >>= assertStatusCode 200 >>= as "relay exists"
    --           egestStats   p1n1 slot1          >>= assertStatusCode 200
    --                                                >>= assertBodyText "1"   >>= as "agent should have 1 client"

    --         it "client requests stream on non-ingest node" do
    --           client start p1n2 slot1          >>= assertStatusCode 404 >>= as "no egest prior to ingest"
    --           relayStats   p1n2 slot1          >>= assertStatusCode 404 >>= as "no remote relay prior to ingest"
    --           ingest start p1n1 shortName1 low >>= assertStatusCode 200 >>= as "create ingest"
    --           delayMs 1000.0
    --           client start p1n2 slot1          >>= assertStatusCode 204 >>= as "egest available"
    --           relayStats   p1n2 slot1          >>= assertStatusCode 200 >>= as "remote relay exists"
    --           egestStats   p1n2 slot1          >>= assertStatusCode 200
    --                                                >>= assertBodyText "1"   >>= as "agent should have 1 client"

    --         it "client requests stream on 2nd node on ingest pop" do
    --           client start p1n2 slot1          >>= assertStatusCode 404 >>= as "no egest p1n2 prior to ingest"
    --           client start p1n3 slot1          >>= assertStatusCode 404 >>= as "no egest p1n3 prior to ingest"
    --           ingest start p1n1 shortName1 low >>= assertStatusCode 200 >>= as "create ingest"
    --           delayMs 1000.0
    --           client start p1n2 slot1          >>= assertStatusCode 204
    --                                                >>= assertHeader (Tuple "x-servedby" "172.16.169.2")
    --                                                                     >>= as "first egest is same node"
    --           delayMs 1000.0
    --           client start p1n3 slot1          >>= assertStatusCode 204
    --                                                >>= assertHeader (Tuple "x-servedby" "172.16.169.2")
    --                                                                     >>= as "p1n3 egest redirects to p1n2"
    --           egestStats   p1n2 slot1          >>= assertStatusCode 200
    --                                                >>= assertBodyText "2"   >>= as "agent should have 2 clients"
    --           egestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "no egest on node3"
    --           client start p1n2 slot1          >>= assertStatusCode 204
    --                                                >>= assertHeader (Tuple "x-servedby" "172.16.169.2")
    --                                                                     >>= as "p1n2 stays on node2"
    --           client start p1n3 slot1          >>= assertStatusCode 204
    --                                                >>= assertHeader (Tuple "x-servedby" "172.16.169.2")
    --                                                                     >>= as "p1n3 egest still redirects to p1n2"
    --           egestStats   p1n2 slot1          >>= assertStatusCode 200
    --                                                >>= assertBodyText "4"   >>= as "agent now has 4 clients"
    --           egestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "still no egest on node3"
    --           client stop  p1n2 slot1          >>= assertStatusCode 204 >>= as "stop client 1 on node2"
    --           client stop  p1n2 slot1          >>= assertStatusCode 204 >>= as "stop client 2 on node2"
    --           client stop  p1n2 slot1          >>= assertStatusCode 204 >>= as "stop client 3 on node2"
    --           client stop  p1n2 slot1          >>= assertStatusCode 204 >>= as "stop client 4 on node2"
    --           delayMs 3000.0                   -- allow the linger timer to expire
    --           egestStats   p1n2 slot1          >>= assertStatusCode 404 >>= as "now no egest on node2"
    --           egestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "still no egest on node3"
    --           client start p1n3 slot1          >>= assertStatusCode 204
    --                                                >>= assertHeader (Tuple "x-servedby" "172.16.169.3")
    --                                                                     >>= as "Final egest starts on node3"
    --           egestStats   p1n3 slot1          >>= assertStatusCode 200
    --                                                >>= assertBodyText "1"   >>= as "node 3 agent should have 1 client"



      -- describe "two pop setup" do
      --   let
      --     p1Nodes = [p1n1, p1n2, p1n3]
      --     p2Nodes = [p2n1]
      --     nodes = p1Nodes <> p2Nodes
      --   before_ (launch nodes) do
      --     after_ stopSession do
      --       it "client requests stream on other pop" do
      --         client start p2n1 slot1           >>= assertStatusCode 404 >>= as "no egest prior to ingest"
      --         relayStats   p1n1 slot1           >>= assertStatusCode 404 >>= as "no remote relay prior to ingest"
      --         relayStats   p1n1 slot1           >>= assertStatusCode 404 >>= as "no local relay prior to ingest"
      --         ingest start p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "create ingest"
      --         delayMs 1000.0
      --         client start p2n1 slot1           >>= assertStatusCode 204 >>= as "egest available"
      --         relayStats   p2n1 slot1           >>= assertStatusCode 200 >>= as "local relay exists"
      --         -- TODO -- relayStatus p1n1 slot1 >>= assertStatusCode 200 >>= as "remote relay exists"

      --       it "client ingest starts and stops" do
      --         client start       p1n2 slot1          >>= assertStatusCode 404 >>= as "no local egest prior to ingest"
      --         client start      p2n1 slot1          >>= assertStatusCode 404 >>= as "no remote egest prior to ingest"
      --         ingest start p1n1 shortName1 low >>= assertStatusCode 200 >>= as "create ingest"
      --         delayMs 1000.0
      --         client start       p1n2 slot1          >>= assertStatusCode 204 >>= as "local egest post ingest"
      --         client start       p2n1 slot1          >>= assertStatusCode 204 >>= as "remote egest post ingest"
      --         ingest stop  p1n1 shortName1 low >>= assertStatusCode 200 >>= as "stop the ingest"
      --         delayMs 5000.0
      --         client start       p1n2 slot1          >>= assertStatusCode 404 >>= as "no same pop egest post stop"
      --         client start       p2n1 slot1          >>= assertStatusCode 404 >>= as "no remote pop egest post stop"

    describe "Cleanup" do
      after_ stopSession do
        it "final cleanup" do
          pure unit


  where
    testConfig = { slow: Milliseconds 5000.0, timeout: Just (Milliseconds 25000.0), exit: false }

assertStatusCode :: Int -> Either String M.Response -> Aff (Either String M.Response)
assertStatusCode expectedCode either =
  pure $ either >>= (\response ->
                      let statusCode = M.statusCode response
                      in
                      if statusCode == expectedCode
                      then either
                      else Left $  "Unexpected statuscode: Expected " <> show expectedCode <> ", got " <> show statusCode
                    )

assertHeader :: Tuple String String -> Either String M.Response -> Aff (Either String M.Response)
assertHeader (Tuple header value) either =
  pure $ either >>= (\response ->
                      let headers = M.headers response
                          mEqual = Object.lookup header headers
                                   >>= (\hdrVal -> if hdrVal == value then Just true
                                                   else Nothing
                                       )

                      in
                       if isNothing mEqual
                       then Left $ "Header " <> header <> ":" <> value <> " not present in response " <> show headers
                       else either
                    )

assertBodyText :: String -> Either String M.Response -> Aff (Either String M.Response)
assertBodyText expected either =
  case either of
    Left e -> pure $ Left e
    Right response -> do
      text <- M.text response
      if text == expected
        then pure either
        else pure $ Left $ "Body " <> text <> " did not match expected " <> expected


assertBodyFun ::  forall a. ReadForeign a => (a -> Boolean) -> Either String M.Response -> Aff (Either String M.Response)
assertBodyFun pred either =
  case either of
    Left e -> pure $ Left e
    Right response -> do
      body <- M.text response
      let
        parsed = SimpleJSON.readJSON body
      case parsed of
        Right a ->
          if pred a
          then pure either
          else
            pure $ Left $ "Predicated failed for body " <> body
        Left _ ->
          pure $ Left $ "Could not parse json " <> body

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

-- jsonBody :: String -> Maybe RequestBody
-- jsonBody string =
--   RequestBody.json <$> hush (Json.jsonParser string)

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

stopNode :: String -> Aff Unit
stopNode nodeAddr = do
  runProc "./scripts/stopNode.sh" [ sessionName
                                  , nodeAddr
                                  , nodeAddr
                                  ]
