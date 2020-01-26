module Main where

import Prelude

import Data.Array (delete, sort)
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
import Shared.Types (ServerAddress(..))
import Shared.Types.Agent.State as PublicState
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

    maybeLogStep s a =
      --let _ = spy s a in
      unit

    as desc (Right r) =
      let _ = maybeLogStep "step" desc in
      pure $ unit
    as desc (Left err) = throwSlowError $ "Step: \"" <> desc <> "\" failed with reason: " <> err

    as' :: forall a. String -> a -> Aff Unit
    as' desc _ =
      let _ = maybeLogStep "step" desc in
      pure $ unit

    debug either = let _ = spy "debug" either in either

    debugBody (Right r) = do
      text <- M.text r
      let _ = spy "debugBody" text
      pure $ Right r
    debugBody (Left err) = let _ = spy "debugBodyErr" err in pure $ Left err

    launch nodes = do
      _ <- stopSession
      nodes <#> mkNode "test/config/sys.config" # launchNodes

    assertRelayForEgest = assertBodyFun <<< predicate
      where
        predicate :: Array Node -> PublicState.StreamRelay -> Boolean
        predicate servers {egestsServed} =
          (sort $ (ServerAddress <<< toAddr) <$> servers) == sort egestsServed

    assertEgestClients = assertBodyFun <<< predicate
      where
        predicate :: Int -> PublicState.Egest -> Boolean
        predicate count {clientCount} = count == clientCount

    assertAggregator = assertBodyFun <<< predicate
      where
        predicate :: Array String -> PublicState.IngestAggregator -> Boolean
        predicate vars {activeStreamVariants} = sort (StreamVariant <$> vars) == (sort $ _.streamVariant <$> activeStreamVariants)

    delayMs = delay <<< Milliseconds

    waitForAsyncVariantStart       = delayMs  100.0
    waitForAsyncVariantStop        = delayMs  100.0

    waitForIntraPoPDisseminate     = delayMs  500.0
    waitForNodeFailureDisseminate  = delayMs 2500.0

    waitForTransPoPDisseminate     = delayMs 2000.0
    waitForTransPoPStopDisseminate = delayMs 5000.0 -- TODO - seeems big

    waitForLessThanLinger          = delayMs  500.0
    waitForMoreThanLinger          = delayMs 1500.0

    waitForMoreThanEgestLinger     = delayMs 3000.0 -- TODO seems big

  in
  launchAff_ $ un Identity $ runSpecT testConfig [consoleReporter] do
    describe "Ingest tests"
      let
        p1Nodes = [p1n1, p1n2, p1n3]
        p2Nodes = [p2n1, p2n2]
        nodes = p1Nodes <> p2Nodes
        allNodesBar node = delete node nodes
        maxOut server = setLoad server 60.0 >>= assertStatusCode 204 >>= as ("set load on " <> toAddr server)
        aggregatorNotPresent slot server = aggregatorStats server slot >>= assertStatusCode 404 >>= as ("aggregator not on " <> toAddr server)
      in do
      before_ (launch nodes) do
        after_ stopSession do
          it "ingest aggregation created on ingest node" do
            ingest start    p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
            waitForAsyncVariantStart                                     >>= as' "wait for async start of variant"
            aggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator has low only"

          it "2nd ingest does not doesn't start new aggregator since one is running" do
            ingest start    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "create low ingest"
            setLoad         p1n1 60.0            >>= assertStatusCode 204 >>= as "set load on server"
            ingest start    p1n1 shortName1 high >>= assertStatusCode 200 >>= as "create high ingest"
            waitForAsyncVariantStart                                      >>= as' "wait for async start of variants"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as "aggregator has 2 variants"

          it "if ingest node is too loaded, then ingest aggregation starts on non-ingest node" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            ingest start    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForIntraPoPDisseminate                                    >>= as' "allow remote ingest location to disseminate"
            ingest start    p1n1 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest"
            waitForAsyncVariantStart                                      >>= as' "wait for async start of variant"
            aggregatorStats p1n2 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as  "aggregator is on p1n2"

          it "ingest on different node removes itself from aggregator when stopped" do
            traverse_ maxOut (allNodesBar p1n2)                          >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                   >>= as' "allow load to disseminate"
            ingest start    p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncVariantStart                                     >>= as' "wait for async start of variant"
            aggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator created on idle server"
            (traverse_ (aggregatorNotPresent slot1) (allNodesBar p1n2))  >>= as' "aggregator not on busy servers"

            ingest stop     p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "stop low ingest"
            waitForAsyncVariantStop                                      >>= as' "wait for async stop of variant"
            aggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                         >>= as  "aggregator has no variants"

          it "ingest restarts aggregator if aggregator exits" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            ingest start    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncVariantStart                                      >>= as' "wait for async start of variant"
            aggregatorStats p1n2 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator created on idle server"
            traverse_ (aggregatorNotPresent slot1) (allNodesBar p1n2)     >>= as' "aggregator not on busy servers"
            setLoad         p1n3 0.0             >>= assertStatusCode 204 >>= as  "mark p1n3 as idle"
            stopNode (toAddr p1n2)                                        >>= as' "make p1n2 fail"
            waitForNodeFailureDisseminate                                 >>= as' "allow failure to disseminate"
            aggregatorStats p1n3 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "failed aggregator moved to new idle server"

          it "aggregator exits after last variant stops (with linger time)" do
            ingest start    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            ingest start    p1n1 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest"
            waitForAsyncVariantStart
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as  "aggregator has both variants"

            ingest stop     p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "stop low ingest"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [high]
                                                                          >>= as  "aggregator only has high"
            ingest stop     p1n1 shortName1 high >>= assertStatusCode 200 >>= as  "stop high ingest"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator []
                                                                          >>= as  "aggregator has no variants"
            waitForMoreThanLinger                                         >>= as' "wait for linger time"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 404 >>= as  "aggregator stops after linger"

          it "aggregator does not exit during linger time" do
            ingest start    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncVariantStart
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator created"
            ingest stop     p1n1 shortName1 low >>= assertStatusCode 200  >>= as  "stop low ingest"
            aggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                          >>= as  "aggregator has no variants"
            waitForLessThanLinger                                         >>= as' "wait for less than the linger time"
            ingest start    p1n2 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest on another node"
            waitForAsyncVariantStart                                      >>= as' "wait for async start of variant"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [high]
                                                                          >>= as  "lingered aggregator has high variant"

    describe "Ingest egest tests" do
      describe "one pop setup"
        let
          p1Nodes = [p1n1, p1n2, p1n3]
          p2Nodes = [p2n1, p2n2]
          nodes = p1Nodes <> p2Nodes
          allNodesBar node = delete node nodes
        in do
        before_ (launch nodes) do
          after_ stopSession do
            it "client requests stream on ingest node" do
              client start p1n1 slot1          >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
              relayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as  "no relay prior to ingest"
              ingest start p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForAsyncVariantStart                                  >>= as' "wait for async start of variant"
              client start p1n1 slot1          >>= assertStatusCode 204 >>= as  "egest available"
              relayStats   p1n1 slot1          >>= assertStatusCode 200
                                                   >>= assertRelayForEgest [p1n1]
                                                                        >>= as  "local relay exists"
              egestStats   p1n1 slot1          >>= assertStatusCode 200
                                                   >>= assertEgestClients 1
                                                                        >>= as "agent should have 1 client"

            it "client requests stream on non-ingest node" do
              client start p1n2 slot1          >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
              relayStats   p1n2 slot1          >>= assertStatusCode 404 >>= as  "no remote relay prior to ingest"
              ingest start p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP source avaialable to disseminate"
              client start p1n2 slot1          >>= assertStatusCode 204 >>= as  "egest available"
              relayStats   p1n2 slot1          >>= assertStatusCode 200 >>= as  "remote relay exists"
              egestStats   p1n2 slot1          >>= assertStatusCode 200
                                                   >>= assertEgestClients 1
                                                                        >>= as "agent should have 1 client"

            it "client requests stream on 2nd node on ingest pop" do
              client start p1n2 slot1          >>= assertStatusCode 404 >>= as "no egest p1n2 prior to ingest"
              client start p1n3 slot1          >>= assertStatusCode 404 >>= as "no egest p1n3 prior to ingest"
              ingest start p1n1 shortName1 low >>= assertStatusCode 200 >>= as "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP source avaialable to disseminate"
              client start p1n2 slot1          >>= assertStatusCode 204
                                                   >>= assertHeader (Tuple "x-servedby" "172.16.169.2")
                                                                        >>= as "first egest is same node"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP egest avaialable to disseminate"
              client start p1n3 slot1          >>= assertStatusCode 204
                                                   >>= assertHeader (Tuple "x-servedby" "172.16.169.2")
                                                                        >>= as "p1n3 egest redirects to p1n2"
              egestStats   p1n2 slot1          >>= assertStatusCode 200
                                                   >>= assertEgestClients 2
                                                                        >>= as "agent should have 2 clients"
              egestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "no egest on node3"
              client start p1n2 slot1          >>= assertStatusCode 204
                                                   >>= assertHeader (Tuple "x-servedby" "172.16.169.2")
                                                                        >>= as "p1n2 stays on node2"
              client start p1n3 slot1          >>= assertStatusCode 204
                                                   >>= assertHeader (Tuple "x-servedby" "172.16.169.2")
                                                                        >>= as "p1n3 egest still redirects to p1n2"
              egestStats   p1n2 slot1          >>= assertStatusCode 200
                                                   >>= assertEgestClients 4
                                                                        >>= as "agent now has 4 clients"
              egestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "still no egest on node3"
              client stop  p1n2 slot1          >>= assertStatusCode 204 >>= as "stop client 1 on node2"
              client stop  p1n2 slot1          >>= assertStatusCode 204 >>= as "stop client 2 on node2"
              client stop  p1n2 slot1          >>= assertStatusCode 204 >>= as "stop client 3 on node2"
              client stop  p1n2 slot1          >>= assertStatusCode 204 >>= as "stop client 4 on node2"

              waitForMoreThanEgestLinger                                >>= as' "allow the egest linger timer to expire"
              egestStats   p1n2 slot1          >>= assertStatusCode 404 >>= as "now no egest on node2"
              egestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "still no egest on node3"
              client start p1n3 slot1          >>= assertStatusCode 204
                                                   >>= assertHeader (Tuple "x-servedby" "172.16.169.3")
                                                                        >>= as "Final egest starts on node3"
              egestStats   p1n3 slot1          >>= assertStatusCode 200
                                                   >>= assertEgestClients 1
                                                                        >>= as "node 3 agent should have 1 client"

      describe "two pop setup" do
        let
          p1Nodes = [p1n1, p1n2, p1n3]
          p2Nodes = [p2n1, p2n2]
          nodes = p1Nodes <> p2Nodes
        before_ (launch nodes) do
          after_ stopSession do
            it "client requests stream on other pop" do
              client start p2n1 slot1          >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
              relayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as  "no remote relay prior to ingest"
              relayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as  "no local relay prior to ingest"
              ingest start p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              client start p2n1 slot1          >>= assertStatusCode 204 >>= as  "egest available"
              relayStats   p2n1 slot1          >>= assertStatusCode 200 >>= as  "local relay exists"
              -- TODO relayStatus p1n1 slot1   >>= assertStatusCode 200 >>= as "remote relay exists"

            it "client ingest starts and stops" do
              client start p1n2 slot1          >>= assertStatusCode 404 >>= as  "no local egest prior to ingest"
              client start p2n1 slot1          >>= assertStatusCode 404 >>= as  "no remote egest prior to ingest"
              ingest start p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              client start p1n2 slot1          >>= assertStatusCode 204 >>= as  "local egest post ingest"
              client start p2n1 slot1          >>= assertStatusCode 204 >>= as  "remote egest post ingest"
              ingest stop  p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "stop the ingest"
              waitForTransPoPStopDisseminate                            >>= as' "wait for transPop disseminate"
              client start p1n2 slot1          >>= assertStatusCode 404 >>= as  "no same pop egest post stop"
              client start p2n1 slot1          >>= assertStatusCode 404 >>= as  "no remote pop egest post stop"

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
  _ <- delay (Milliseconds 200.0)
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
