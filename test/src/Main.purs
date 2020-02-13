module Main where

import Prelude

import Data.Array (catMaybes, delete, filter, intercalate, length, sort)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (un, wrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, launchAff_, throwError)
import Effect.Exception (error) as Exception
import Foreign.Object as Object
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import OsCmd (runProc)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Union)
import Shared.Stream (StreamVariant(..))
import Shared.Types (ServerAddress(..), extractAddress)
import Shared.Types.Agent.State as PublicState
import Simple.JSON (class ReadForeign)
import Simple.JSON as SimpleJSON
import Test.Spec (after_, before_, describe, describeOnly, it, itOnly)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT)


type ResponseWithBody =
  { headers :: M.Headers
  , body :: String
  , statusCode :: Int
  }


fetch :: forall options trash.
  Union options trash M.Options
   => M.URL -> Record (method :: M.Method | options) -> Aff (Either String ResponseWithBody)
fetch url options = M.fetch nodeFetch url options # attempt >>= standarise
 where
   standarise (Right response) = do
     body <- M.text response
     pure $ Right { headers   : M.headers response
                  , statusCode : M.statusCode response
                  , body
                  }
   standarise (Left axError) = pure $ Left $ show axError



get :: M.URL -> Aff (Either String ResponseWithBody)
get url = fetch url { method: M.getMethod }


type TestNode =  { ifaceIndexString :: String
                 , addr :: String
                 , sysConfig :: String
                 }

data Node = Node Int Int
derive instance eqNode :: Eq Node


type PoPInfo = { name :: String
               , number :: Int
               , x :: Number
               , y :: Number
               }

mkPoPJson :: Array Node -> String
mkPoPJson  nodes =
  let

    iad = {name: "iad", number: 1, x: 0.0, y: 0.0} :: PoPInfo
    dal = {name: "dal", number: 2, x: 0.0, y: 0.0} :: PoPInfo
    lax = {name: "lax", number: 3, x: 0.0, y: 0.0} :: PoPInfo
    fra = {name: "fra", number: 4, x: 0.0, y: 0.0} :: PoPInfo

    popNodes pop = filter (\(Node popNum  _) -> popNum == pop.number)

    insertNodes k [] acc = acc
    insertNodes k v acc = Map.insert k v acc

    nodeMap = Map.empty
              # insertNodes iad.number (popNodes iad nodes)
              # insertNodes dal.number (popNodes dal nodes)
              # insertNodes lax.number (popNodes lax nodes)
              # insertNodes fra.number (popNodes fra nodes)

    quote = show
    mkPoP :: PoPInfo -> Maybe String
    mkPoP pop =
      case Map.lookup pop.number nodeMap of
        Nothing -> Nothing
        Just nodesThisPoP -> Just $
          show pop.name <> ": { \"geoLoc\" : [" <> (quote $ show pop.x) <> "," <> (quote $ show pop.y) <> "], \"nodes\": ["
                    <> intercalate ", " (quote <<< toAddr <$> nodesThisPoP)
                    <> "]}"

    mkRegion :: String -> (Array String) -> Maybe String
    mkRegion name [] = Nothing
    mkRegion name pops = Just $
      quote name <> ": {"
                 <> intercalate ", " pops
                 <> "}"

    america = mkRegion "americas" $ catMaybes $ mkPoP <$> [iad, dal, lax]
    europe  = mkRegion "europe"   $ catMaybes $ mkPoP <$> [fra]

  in
   "{"  <> (intercalate ", " $ catMaybes [america, europe]) <> "}"


forceRight :: forall a b. Either a b -> b
forceRight e = unsafePartial $ case e of
  Right b -> b



main :: Effect Unit
main =
  let

    p1n1 = Node 1 1
    p1n2 = Node 1 2
    p1n3 = Node 1 3
    p2n1 = Node 2 1
    p2n2 = Node 2 2
    p3n1 = Node 3 1
    p3n2 = Node 3 2
    p4n1 = Node 4 1
    p4n2 = Node 4 2

    slot1      = "slot1"
    shortName1 = "mmddev001"
    low        = "slot1_500"
    high       = "slot1_1000"

    start = "start"
    stop  = "stop"

    api node = "http://" <> toAddr node <> ":3000/api/"

    egestStats node streamId           = get (M.URL $ api node <> "agents/egest/" <> streamId)

    aggregatorStats node streamId      = get (M.URL $ api node <> "agents/ingestAggregator/" <> streamId <> "/primary")

    ingestStart node shortName variant = get (M.URL $ api node <> "public/canary/ingest/" <> shortName <> "/" <> variant <> "/start")
    ingestStop node streamId variant = get (M.URL $ api node <> "public/canary/ingest/" <> streamId <> "/primary/" <> variant <> "/stop")

    relayStats node streamId           = get (M.URL $ api node <> "agents/relay/" <> streamId)

    proxiedRelayStats node streamId    = get (M.URL $ api node <> "agents/proxied/relay/" <> streamId)

    intraPoPState node                 = get (M.URL $ api node <> "state")

    client verb node streamId           = fetch (M.URL $ api node <> "public/canary/client/" <> streamId <> "/" <> verb)
                                         { method: M.postMethod
                                         , body: "{}"
                                         , headers: M.makeHeaders { "Content-Type": "application/json" }
                                         }

    setLoad node load                  = fetch (M.URL $ api node <> "load")
                                         { method: M.postMethod
                                         , body: "{\"load\": " <> show load <> "}"
                                         , headers: M.makeHeaders { "Content-Type": "application/json" }
                                         }

    dropAgentMessages node flag        = fetch (M.URL $ api node <> "test/intraPoP")
                                         { method: M.postMethod
                                         , body: "{\"dropAgentMessages\": " <> show flag <> "}"
                                         , headers: M.makeHeaders { "Content-Type": "application/json" }
                                         }

    maybeLogStep s a =
      let _ = spy s a in
      unit

    as :: forall r. String -> Either String r -> Aff Unit
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


    assertRelayForEgest = assertBodyFun <<< predicate
      where
        predicate :: Array Node -> PublicState.StreamRelay Array -> Boolean
        predicate servers {egestsServed} =
          (sort $ (ServerAddress <<< toAddr) <$> servers) == sort egestsServed

    assertRelayForRelay = assertBodyFun <<< predicate
      where
        predicate :: Array Node -> PublicState.StreamRelay Array -> Boolean
        predicate servers {relaysServed} =
          (sort $ (ServerAddress <<< toAddr) <$> servers) == sort relaysServed

    assertEgestClients = assertBodyFun <<< predicate
      where
        predicate :: Int -> PublicState.Egest -> Boolean
        predicate count {clientCount} = count == clientCount

    assertAggregator = assertBodyFun <<< predicate
      where
        predicate :: Array String -> PublicState.IngestAggregator Array -> Boolean
        predicate vars {activeStreamVariants} = sort (StreamVariant <$> vars) == (sort $ _.streamVariant <$> activeStreamVariants)

    assertAggregatorOn nodes slotName  = assertBodyFun $ predicate
      where
        predicate :: PublicState.IntraPoP Array -> Boolean
        predicate popState =
          let
            nodeAddresses = toAddr
            serverAddressesForStreamId = foldl (\acc {streamId, servers} ->
                                                 if streamId == wrap slotName
                                                 then acc <> (extractAddress <$> servers)
                                                 else acc
                                               ) []  popState.aggregatorLocations
          in
          (sort $ (ServerAddress <<< toAddr) <$> nodes) == sort serverAddressesForStreamId

    assertRelayCount slotName count = assertBodyFun $ predicate
      where
        predicate :: (PublicState.IntraPoP Array) -> Boolean
        predicate popState =
          let
            nodeAddresses = toAddr
            serverAddressesForStreamId = foldl (\acc {streamId, servers} ->
                                                 if streamId == wrap slotName
                                                 then acc <> (extractAddress <$> servers)
                                                 else acc
                                               ) []  popState.relayLocations
          in
          length (sort serverAddressesForStreamId) == count

    forceGetState node = forceRight <$> intraPoPState node

    assertBodiesSame :: List ResponseWithBody -> Aff (Either String Unit)
    assertBodiesSame Nil = pure $ Right unit
    assertBodiesSame (Cons x xs) = pure $ assertBodiesSame_ x.body xs
      where
        assertBodiesSame_ :: String -> List ResponseWithBody -> Either String Unit
        assertBodiesSame_ _ Nil = Right unit
        assertBodiesSame_ firstBody (Cons y ys) =
          if y.body == firstBody
          then assertBodiesSame_ firstBody ys
          else Left $ "\"" <> firstBody <> "\" is not the same as \"" <> y.body <> "\""


    delayMs = delay <<< Milliseconds

    waitForMessageTimeout          = delayMs 2000.0

    waitForAsyncRelayStart         = delayMs  100.0
    waitForAsyncRelayStop          = delayMs  100.0

    waitForAsyncVariantStart       = delayMs  100.0
    waitForAsyncVariantStop        = delayMs  100.0

    waitForIntraPoPDisseminate     = delayMs  500.0

    waitForNodeStartDisseminate    = delayMs 1000.0
    waitForNodeFailureDisseminate  = delayMs 3500.0 -- TODO - seems big

    waitForTransPoPDisseminate     = delayMs 2000.0
    waitForTransPoPStopDisseminate = delayMs 5000.0 -- TODO - seeems big

    waitForLessThanLinger          = delayMs  500.0
    waitForMoreThanLinger          = delayMs 1500.0

    waitForMoreThanEgestLinger     = delayMs 3000.0 -- TODO seems big

  in
  launchAff_ $ un Identity $ runSpecT testConfig [consoleReporter] do
    describe "Startup tests"
      let
        p1Nodes = [p1n1, p1n2, p1n3]
        nodes = p1Nodes
      in do
      before_ (do
                 startSession nodes
                 launch nodes
              ) do
        after_ stopSession do
          it "Nodes all come up and agree on who the leader is" do
            states <- traverse forceGetState (Array.toUnfoldable p1Nodes)
            assertBodiesSame states                                      >>= as "All nodes agree on leader and other intial state"

    describe "Ingest tests"
      let
        p1Nodes = [p1n1, p1n2, p1n3]
        nodes = p1Nodes
        allNodesBar node = delete node nodes
        maxOut server = setLoad server 60.0 >>= assertStatusCode 204 >>= as ("set load on " <> toAddr server)
        aggregatorNotPresent slot server = aggregatorStats server slot >>= assertStatusCode 404 >>= as ("aggregator not on " <> toAddr server)
      in do
      before_ (do
                 startSession nodes
                 launch nodes
              ) do
        after_ stopSession do
          it "ingest aggregation created on ingest node" do
            ingestStart    p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
            waitForAsyncVariantStart                                     >>= as' "wait for async start of variant"
            aggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator has low only"

          it "2nd ingest does not doesn't start new aggregator since one is running" do
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "create low ingest"
            setLoad         p1n1 60.0            >>= assertStatusCode 204 >>= as "set load on server"
            ingestStart    p1n1 shortName1 high >>= assertStatusCode 200 >>= as "create high ingest"
            waitForAsyncVariantStart                                      >>= as' "wait for async start of variants"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as "aggregator has 2 variants"

          it "if ingest node is too loaded, then ingest aggregation starts on non-ingest node" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForIntraPoPDisseminate                                    >>= as' "allow remote ingest location to disseminate"
            ingestStart    p1n1 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest"
            waitForAsyncVariantStart                                      >>= as' "wait for async start of variant"
            aggregatorStats p1n2 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as  "aggregator is on p1n2"

          it "ingest on different node removes itself from aggregator when stopped" do
            traverse_ maxOut (allNodesBar p1n2)                          >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                   >>= as' "allow load to disseminate"
            ingestStart    p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncVariantStart                                     >>= as' "wait for async start of variant"
            aggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator created on idle server"
            (traverse_ (aggregatorNotPresent slot1) (allNodesBar p1n2))  >>= as' "aggregator not on busy servers"

            ingestStop     p1n1 slot1 low >>= assertStatusCode 200 >>= as  "stop low ingest"
            waitForAsyncVariantStop                                      >>= as' "wait for async stop of variant"
            aggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                         >>= as  "aggregator has no variants"

          it "ingest restarts aggregator if aggregator exits" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncVariantStart                                      >>= as' "wait for async start of variant"
            aggregatorStats p1n2 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator created on idle server"
            traverse_ (aggregatorNotPresent slot1) (allNodesBar p1n2)     >>= as' "aggregator not on busy servers"
            setLoad         p1n3 0.0             >>= assertStatusCode 204 >>= as  "mark p1n3 as idle"
            stopNode p1n2                        >>= as' "make p1n2 fail"
            waitForNodeFailureDisseminate                                 >>= as' "allow failure to disseminate"
            aggregatorStats p1n3 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "failed aggregator moved to new idle server"

          it "aggregator exits after last variant stops (with linger time)" do
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            ingestStart    p1n1 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest"
            waitForAsyncVariantStart
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as  "aggregator has both variants"

            ingestStop     p1n1 slot1 low  >>= assertStatusCode 200 >>= as  "stop low ingest"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [high]
                                                                          >>= as  "aggregator only has high"
            ingestStop     p1n1 slot1 high >>= assertStatusCode 200 >>= as  "stop high ingest"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator []
                                                                          >>= as  "aggregator has no variants"
            waitForMoreThanLinger                                         >>= as' "wait for linger time"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 404 >>= as  "aggregator stops after linger"

          it "aggregator does not exit during linger time" do
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncVariantStart
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator created"
            ingestStop     p1n1 slot1 low >>= assertStatusCode 200  >>= as  "stop low ingest"
            aggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                          >>= as  "aggregator has no variants"
            waitForLessThanLinger                                         >>= as' "wait for less than the linger time"
            ingestStart    p1n2 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest on another node"
            waitForAsyncVariantStart                                      >>= as' "wait for async start of variant"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [high]
                                                                          >>= as  "lingered aggregator has high variant"

          it "aggregator liveness detected on node stop" do
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForIntraPoPDisseminate
            intraPoPState p1n1                   >>= assertAggregatorOn [p1n1] slot1
                                                                          >>= as "p1n1 is aware of the ingest on p1n1"
            intraPoPState p1n2                   >>= assertAggregatorOn [p1n1] slot1
                                                                          >>= as "p1n2 is aware of the ingest on p1n1"
            stopNode p1n1                        >>= as' "make p1n1 fail"
            waitForNodeFailureDisseminate                                 >>= as' "allow failure to disseminate"
            intraPoPState p1n2                   >>= assertAggregatorOn [] slot1
                                                                          >>= as "p1n2 is aware the ingest stopped"

    describe "Ingest egest tests" do
      describe "one pop setup"
        let
          p1Nodes = [p1n1, p1n2, p1n3]
          nodes = p1Nodes
          allNodesBar node = delete node nodes
        in do
        before_ (do
                   startSession nodes
                   launch nodes
                ) do
          after_ stopSession do
            it "client requests stream on ingest node" do
              client start p1n1 slot1          >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
              relayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as  "no relay prior to ingest"
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
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
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP source avaialable to disseminate"
              client start p1n2 slot1          >>= assertStatusCode 204 >>= as  "egest available"
              relayStats   p1n2 slot1          >>= assertStatusCode 200 >>= as  "remote relay exists"
              egestStats   p1n2 slot1          >>= assertStatusCode 200
                                                   >>= assertEgestClients 1
                                                                        >>= as "agent should have 1 client"

            it "client requests stream on 2nd node on ingest pop" do
              client start p1n2 slot1          >>= assertStatusCode 404 >>= as "no egest p1n2 prior to ingest"
              client start p1n3 slot1          >>= assertStatusCode 404 >>= as "no egest p1n3 prior to ingest"
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as "create ingest"
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
        before_ (do
                   startSession nodes
                   launch nodes
                ) do
          after_ stopSession do
            it "aggregator presence is disseminated to all servers" do
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200     >>= as  "create ingest"
              waitForTransPoPDisseminate                                    >>= as' "wait for transPop disseminate"
              intraPoPState p1n1                   >>= assertAggregatorOn [p1n1] slot1
                                                                            >>= as "p1n1 is aware of the ingest on p1n1"
              states1 <- traverse forceGetState (Array.toUnfoldable p1Nodes)
              assertBodiesSame states1                                      >>= as "All pop 1 nodes agree on leader and aggregator presence"
              states2 <- traverse forceGetState (Array.toUnfoldable p2Nodes)
              assertBodiesSame states2                                      >>= as "All pop 2 nodes agree on leader and aggregator presence"

            it "client requests stream on other pop" do
              client start p2n1 slot1          >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
              relayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as  "no remote relay prior to ingest"
              relayStats   p2n1 slot1          >>= assertStatusCode 404 >>= as  "no local relay prior to ingest"
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              client start p2n1 slot1          >>= assertStatusCode 204 >>= as  "egest available"
              relayStats   p2n1 slot1          >>= assertStatusCode 200 >>= as  "local relay exists"
              waitForAsyncRelayStart                                    >>= as' "wait for the relay chain to start"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP to spread location of relay"
              intraPoPState p1n1               >>= assertStatusCode 200
                                                   >>= assertRelayCount slot1 1
                                                                        >>= as  "relay created in aggregator pop"
              proxiedRelayStats p1n1 slot1     >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p2n1]
                                                                        >>= as  "remote relay is serving local relay"

            it "client ingest starts and stops" do
              client start p1n2 slot1          >>= assertStatusCode 404 >>= as  "no local egest prior to ingest"
              client start p2n1 slot1          >>= assertStatusCode 404 >>= as  "no remote egest prior to ingest"
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              client start p1n2 slot1          >>= assertStatusCode 204 >>= as  "local egest post ingest"
              client start p2n1 slot1          >>= assertStatusCode 204 >>= as  "remote egest post ingest"
              ingestStop  p1n1 slot1 low >>= assertStatusCode 200 >>= as  "stop the ingest"
              waitForTransPoPStopDisseminate                            >>= as' "wait for transPop disseminate"
              client start p1n2 slot1          >>= assertStatusCode 404 >>= as  "no same pop egest post stop"
              client start p2n1 slot1          >>= assertStatusCode 404 >>= as  "no remote pop egest post stop"
              -- TODO - assert the relays stop as well - might be slow with timeouts chaining...


      describe "node startup - one pop" do
        let
          phase1Nodes = [p1n1, p1n2]
          phase2Nodes = [p1n3]
          nodes = phase1Nodes <> phase2Nodes
          sysconfig = "test/config/partial_nodes/sys.config"
        before_ (do
                   startSession nodes
                   launch' phase1Nodes sysconfig
                ) do
          after_ stopSession do
            it "a node that starts late gets to see existing streams" do
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "let ingest presence disseminate"
              launch' phase2Nodes sysconfig                             >>= as' "start new node after ingest already running"
              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"
              client start p1n3 slot1          >>= assertStatusCode 204 >>= as  "local egest post ingest"

            -- TODO - egest - test stream we think is not present when it is

      describe "packet loss - one pop" do
        let
          nodes = [p1n1, p1n2]
          sysconfig = "test/config/partial_nodes/sys.config"
        before_ (do
                   startSession nodes
                   launch' nodes sysconfig
                ) do
          after_ stopSession do
            it "aggregator expired after extended packet loss" do
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "let ingest presence disseminate"
              client start p1n2 slot1          >>= assertStatusCode 204 >>= as  "local egest post ingest"
              dropAgentMessages p1n2 true                               >>= as  "Drop all agent messages"
              waitForIntraPoPDisseminate                                >>= as' "Wait for less than message expiry"
              client start p1n2 slot1          >>= assertStatusCode 204 >>= as  "Initially clients can still join"
              waitForMessageTimeout                                     >>= as' "Wait for less than message expiry"
              client start p1n2 slot1          >>= assertStatusCode 404 >>= as  "Clients can no longer join"
              dropAgentMessages p1n2 false                              >>= as  "Alow messages to flow once more"

              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"
              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"

              client start p1n2 slot1          >>= assertStatusCode 204 >>= as  "Client can join once more"


      describe "four pop setup" do
        let
          p1Nodes = [p1n1]  -- iad
          p2Nodes = [p2n1]  -- dal
          p3Nodes = [p3n1]  -- fra
          p4Nodes = [p4n1]  -- lax
          -- the topology in wanDefinition.json is important - maybe make explicit for this test...
          -- todo - why don't singleton pops work?
          nodes = p1Nodes <> p2Nodes <> p3Nodes <> p4Nodes

        before_ (do
                   startSession nodes
                   launch nodes
                ) do
          after_ stopSession do
            it "lax -> fra sets up 2 non-overlapping relay chains" do
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP to spread location of relay"
              ingestStart p3n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              client start p4n1 slot1          >>= assertStatusCode 204 >>= as  "egest available in lax"
              relayStats   p4n1 slot1          >>= assertStatusCode 200 >>= as  "local relay exists"
              waitForAsyncRelayStart                                    >>= as' "wait for the relay chain to start"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP to spread location of relay"
              relayStats p1n1 slot1            >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p4n1]
                                                       >>= assertRelayForEgest []
                                                                        >>= as  "iad relays for lax with no egests of its own"

              relayStats p2n1 slot1            >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p4n1]
                                                       >>= assertRelayForEgest []
                                                                        >>= as  "dal relays for lax with no egests of its own"
              relayStats p3n1 slot1            >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p1n1, p2n1]
                                                       >>= assertRelayForEgest []
                                                                        >>= as  "fra relays for both iad and dal with no egests of its own"

    describe "Cleanup" do
      after_ stopSession do
        it "final cleanup" do
          pure unit


  where
    testConfig = { slow: Milliseconds 5000.0, timeout: Just (Milliseconds 25000.0), exit: false }

assertStatusCode :: Int -> Either String ResponseWithBody -> Aff (Either String ResponseWithBody)
assertStatusCode expectedCode either =
  pure $ either >>= (\rwb ->
                      if rwb.statusCode == expectedCode
                      then either
                      else Left $  "Unexpected statuscode: Expected " <> show expectedCode <> ", got " <> show rwb.statusCode
                    )

assertHeader :: Tuple String String -> Either String ResponseWithBody -> Aff (Either String ResponseWithBody)
assertHeader (Tuple header value) either =
  pure $ either >>= (\rwb@{headers} ->
                      let
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


assertBodyFun ::  forall a. ReadForeign a => (a -> Boolean) -> Either String ResponseWithBody -> Aff (Either String ResponseWithBody)
assertBodyFun pred either =
  case either of
    Left e -> pure $ Left e
    Right rwb -> do
      let
        parsed = SimpleJSON.readJSON rwb.body
      case parsed of
        Right a ->
          if pred a
          then pure either
          else
            pure $ Left $ "Predicate failed for body " <> rwb.body
        Left _ ->
          pure $ Left $ "Could not parse json " <> rwb.body

toAddr :: Node -> String
toAddr (Node popNum nodeNum) = "172.16." <> show (popNum + 168) <> "." <> show nodeNum

toIfaceIndexString :: Node -> String
toIfaceIndexString (Node popNum nodeNum) = show (popNum * 10) <> show nodeNum

mkNode :: String  -> Node -> TestNode
mkNode sysConfig node = {ifaceIndexString: toIfaceIndexString node, addr: toAddr node, sysConfig}


sessionName:: String
sessionName = "testSession"


startSession :: Array Node -> Aff Unit
startSession allNodes = do
  stopSession
  writeTextFile UTF8 "config/popDefinition.json" $ mkPoPJson allNodes

  runProc "./scripts/startSession.sh" [sessionName]


launch :: Array Node -> Aff Unit
launch nodes = launch' nodes "test/config/sys.config"

launch' :: Array Node -> String -> Aff Unit
launch' nodesToStart sysconfig = do
  nodesToStart <#> mkNode  sysconfig # launchNodes
  where
  launchNodes :: Array TestNode -> Aff Unit
  launchNodes nodes = do
    traverse_ (\tn -> runProc "./scripts/startNode.sh"
                      [ sessionName
                      , tn.addr
                      , tn.ifaceIndexString
                      , tn.addr
                      , tn.sysConfig
                      ]) nodes

    traverse_ (\tn -> runProc "./scripts/waitForNode.sh"
                      [ tn.addr
                      ]) nodes

stopSession :: Aff Unit
stopSession = do
  _ <- delay (Milliseconds 200.0)
  runProc "./scripts/stopSession.sh" [sessionName]


throwSlowError :: forall e. String -> Aff e
throwSlowError e =
  do
    _ <- delay (Milliseconds 200.0)
    throwError $ Exception.error e

stopNode :: Node -> Aff Unit
stopNode node = do
  let nodeAddr = toAddr node
  runProc "./scripts/stopNode.sh" [ sessionName
                                  , nodeAddr
                                  , nodeAddr
                                  ]
