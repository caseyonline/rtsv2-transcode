module Main where

import Prelude

import Data.Array (catMaybes, delete, filter, intercalate, length, sort, sortBy, head)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), fromRight)
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Lens (Lens', Traversal', _Just, firstOf, over, set, traversed)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing, fromMaybe, fromMaybe')
import Data.Newtype (class Newtype, un, wrap, unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
import Data.These
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, launchAff_, throwError)
import Effect.Exception (error) as Exception
import Foreign.Object as Object
import Foreign (F, MultipleErrors)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import OsCmd (runProc)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Union)
import Shared.Chaos as Chaos
import Shared.Stream (ProfileName(..), SlotId, SlotRole(..), SlotNameAndProfileName(..))
import Shared.Router.Endpoint (Endpoint(..), makeUrl, makeUrlAddr, Canary(..))
import Shared.Common (Url)
import Shared.Types (ServerAddress(..), extractAddress)
import Shared.Types.Agent.State as PublicState
import Shared.Utils (lazyCrashIfMissing)
import Shared.UUID (fromString)
import Shared.Rtsv2.JsonLd as JsonLd
import Simple.JSON (class ReadForeign, E)
import Simple.JSON as SimpleJSON
import Test.Spec (after_, before_, describe, describeOnly, it, itOnly)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT)
import Text.Parsing.Parser
import URI as URI
import URI.HierarchicalPart (HierarchicalPart(..))
import URI.Authority (Authority(..))
import URI.HostPortPair as HostPortPair
import URI.Path (Path(..))
import URI.Host as URIHost
import URI.HostPortPair as URIHostPortPair
import URI.URI as URIParser
import Control.Monad.State
import Control.Monad.State.Class

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

newtype NodeAddress = NodeAddress {address :: ServerAddress}
derive instance newtypeNodeAddress :: Newtype NodeAddress _

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

    slot1      = wrap (fromMaybe' (lazyCrashIfMissing "Invalid UUID") (fromString "00000000-0000-0000-0000-000000000001"))
    shortName1 = wrap "mmddev001"
    low        = SlotNameAndProfileName "slot1" (wrap "500")
    high       = SlotNameAndProfileName "slot1" (wrap "1000")

    stop  = "stop"

    serverAddress node = NodeAddress {address: ServerAddress $ toAddr node}

    makeUrl' node path = unwrap $ makeUrl (serverAddress node) path

    egestStats node slotId           = get (M.URL $ makeUrl' node (EgestStatsE slotId Primary))

    aggregatorStats node slotId      = get (M.URL $ makeUrl' node (IngestAggregatorE slotId Primary))

    ingestStart node shortName profileName = get (M.URL $ makeUrl' node (IngestStartE Live shortName profileName))

    ingestStop node slotId (SlotNameAndProfileName _ profileName)     = get (M.URL $ makeUrl' node (IngestStopE Live slotId Primary profileName))

    relayStats node slotId           = get (M.URL $ makeUrl' node (RelayStatsE slotId Primary))

    proxiedRelayStats node slotId    = get (M.URL $ makeUrl' node (RelayProxiedStatsE slotId Primary))

    intraPoPState node               = get (M.URL $ makeUrl' node ServerStateE)

    slotState node slotId            = get (M.URL $ makeUrl' node (SlotStateE slotId))

    ingestAggregatorName slotId role = Chaos.Gproc (Chaos.GprocTuple2 (Chaos.String "IngestAggregator") (Chaos.GprocTuple3 (Chaos.Atom "aggregatorKey") (Chaos.SlotId slotId) (Chaos.SlotRole role)))

    ingestName slotId role streamName = Chaos.Gproc (Chaos.GprocTuple2 (Chaos.String "Ingest") (Chaos.GprocTuple4 (Chaos.Atom "ingestKey") (Chaos.SlotId slotId) (Chaos.SlotRole role) (Chaos.String streamName)))

    killProcess node chaos           = fetch (M.URL $ makeUrl' node (Chaos))
                                         { method: M.postMethod
                                         , body: SimpleJSON.writeJSON (chaos :: Chaos.ChaosPayload)
                                         , headers: M.makeHeaders { "Content-Type": "application/json" }
                                         }

    killProcess' addr chaos          = fetch (M.URL $ unwrap $ makeUrlAddr addr (Chaos))
                                         { method: M.postMethod
                                         , body: SimpleJSON.writeJSON (chaos :: Chaos.ChaosPayload)
                                         , headers: M.makeHeaders { "Content-Type": "application/json" }
                                         }

    clientStart node slotId          = fetch (M.URL $ makeUrl' node (ClientStartE Live slotId Primary))
                                         { method: M.postMethod
                                         , body: "{}"
                                         , headers: M.makeHeaders { "Content-Type": "application/json" }
                                         }

    clientStop clientId node slotId  = fetch (M.URL $ makeUrl' node (ClientStopE Live slotId Primary clientId))
                                         { method: M.postMethod
                                         , body: "{}"
                                         , headers: M.makeHeaders { "Content-Type": "application/json" }
                                         }

    setLoad node load                 = fetch (M.URL $ makeUrl' node LoadE)
                                          { method: M.postMethod
                                          , body: "{\"load\": " <> show load <> "}"
                                          , headers: M.makeHeaders { "Content-Type": "application/json" }
                                          }

    dropAgentMessages node flag       = fetch (M.URL $ makeUrl' node IntraPoPTestHelperE)
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

    asT :: forall r s. String -> Either String r -> StateT s Aff Unit
    asT desc (Right r) =
      let _ = maybeLogStep "step" desc in
      pure $ unit
    asT desc (Left err) = lift $ throwSlowError $ "Step: \"" <> desc <> "\" failed with reason: " <> err

    asT' :: forall a b. String -> b -> StateT a Aff Unit
    asT' desc _ =
      let _ = maybeLogStep "step" desc in
      pure $ unit

    debug :: forall a b. String -> Either a b -> Aff (Either a b)
    debug msg either =
      let
        _ = spy msg either
      in pure $ either

    debugBody (Right r) = do
      text <- M.text r
      let _ = spy "debugBody" text
      pure $ Right r
    debugBody (Left err) = let _ = spy "debugBodyErr" err in pure $ Left err

    assertRelayForEgest = assertBodyFun <<< predicate
      where
        predicate :: Array Node -> PublicState.StreamRelay Array -> Boolean
        predicate servers streamRelayState =
          (sort $ (ServerAddress <<< toAddr) <$> servers) == sort (_.address <<< JsonLd.unwrapNode <$> (JsonLd.unwrapNode streamRelayState).egestsServed)

    assertRelayForRelay = assertBodyFun <<< predicate
      where
        predicate :: Array Node -> PublicState.StreamRelay Array -> Boolean
        predicate servers streamRelayState =
          (sort $ (ServerAddress <<< toAddr) <$> servers) == sort (_.address <<< unwrap <<< _.server <<< JsonLd.unwrapNode <$> (JsonLd.unwrapNode streamRelayState).relaysServed)

    assertEgestClients = assertBodyFun <<< predicate
      where
        predicate :: Int -> PublicState.Egest -> Boolean
        predicate count egestStats = count == (JsonLd.unwrapNode egestStats).clientCount

    assertAggregator = assertBodyFun <<< predicate
      where
        predicate :: Array SlotNameAndProfileName -> PublicState.IngestAggregator Array -> Boolean
        predicate vars ingestAggregatorState =
          sort ((\(SlotNameAndProfileName _ profileName) -> profileName) <$> vars) == (sort $ (_.profileName <<< JsonLd.unwrapNode) <$> (JsonLd.unwrapNode ingestAggregatorState).activeProfiles)

    dumpIntraPoP = assertBodyFun $ predicate
      where
        predicate :: PublicState.IntraPoP Array -> Boolean
        predicate popState =
          let
            _ = spy "popState" popState
          in
           true

    dumpSlotState = assertBodyFun $ predicate
      where
        predicate :: PublicState.SlotState Array -> Boolean
        predicate slotState =
          let
            _ = spy "slotState" slotState
          in
           true

    assertAggregatorOn nodes requiredSlotId  = assertBodyFun $ predicate
      where
        predicate :: PublicState.IntraPoP Array -> Boolean
        predicate popState =
          let
            nodeAddresses = toAddr
            serverAddressesForSlotId = foldl (\acc {slotId, servers} ->
                                                 if slotId == requiredSlotId
                                                 then acc <> (extractAddress <<< JsonLd.unwrapNode <$> servers)
                                                 else acc
                                               ) []  (JsonLd.unwrapNode popState).aggregatorLocations
          in
          (sort $ (ServerAddress <<< toAddr) <$> nodes) == sort serverAddressesForSlotId

    assertRelayCount requiredSlotId count = assertBodyFun $ predicate
      where
        predicate :: (PublicState.IntraPoP Array) -> Boolean
        predicate popState =
          let
            nodeAddresses = toAddr
            serverAddressesForSlotId = foldl (\acc {slotId, servers} ->
                                                 if slotId == requiredSlotId
                                                 then acc <> (extractAddress <<< JsonLd.unwrapNode <$> servers)
                                                 else acc
                                               ) []  (JsonLd.unwrapNode popState).relayLocations
          in
          length (sort serverAddressesForSlotId) == count

    forceGetState :: Node -> Aff (JsonLd.IntraPoPState Array)
    forceGetState node = JsonLd.unwrapNode <$> forceRight <$> (jsonToType' :: ResponseWithBody -> Either String (PublicState.IntraPoP Array)) <$> forceRight <$> intraPoPState node

    assertSame :: forall a. Show a => Eq a => List a -> Aff (Either String Unit)
    assertSame Nil = pure $ Right unit
    assertSame (Cons x Nil) = pure $ Right unit
    assertSame (Cons x t@(Cons y xs)) = if x == y then assertSame t
                                        else pure $ Left $ "\"" <> (show x) <> "\" is not the same as \"" <> (show y) <> "\""

    jsonToType' :: forall a. ReadForeign a => ResponseWithBody -> Either String a
    jsonToType' {body} = lmap (const "Failed to parse JSON") $ SimpleJSON.readJSON body

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

    waitForSupervisorRecovery      = delayMs  50.0

    waitForAsyncRelayStart         = delayMs  100.0
    waitForAsyncRelayStop          = delayMs  100.0

    waitForAsyncProfileStart       = delayMs  150.0
    waitForAsyncProfileStop        = delayMs  100.0

    waitForRemoteAsyncProfileStart = delayMs  350.0

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
    describe "1 Startup tests"
      let
        p1Nodes = [p1n1, p1n2, p1n3]
        nodes = p1Nodes
      in do
      before_ (do
                 startSession nodes
                 launch nodes
              ) do
        after_ stopSession do
          it "1.1 Nodes all come up and agree on who the leader is" do
            states <- traverse forceGetState (Array.toUnfoldable p1Nodes) :: Aff (List (JsonLd.IntraPoPState Array))
            let
              leaders = JsonLd.unwrapNode <$> List.catMaybes (_.currentTransPoPLeader <$> states)
            assertSame leaders               >>= as "All nodes agree on leader and other intial state"

    describe "2 Ingest tests"
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
          it "2.1 ingest aggregation created on ingest node" do
            ingestStart    p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
            waitForAsyncProfileStart                                     >>= as' "wait for async start of profile"
            aggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator has low only"

          it "2.2 2nd ingest doesn't start new aggregator since one is running" do
            ingestStart    p1n1 shortName1 low   >>= assertStatusCode 200 >>= as "create low ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            setLoad         p1n1 60.0            >>= assertStatusCode 204 >>= as "set load on server"
            ingestStart    p1n1 shortName1 high  >>= assertStatusCode 200 >>= as "create high ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as "aggregator has 2 profiles"

          it "2.3 if ingest node is too loaded, then ingest aggregation starts on non-ingest node" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForIntraPoPDisseminate                                    >>= as' "allow remote ingest location to disseminate"
            ingestStart    p1n1 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            aggregatorStats p1n2 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as  "aggregator is on p1n2"

          it "2.4 ingest on different node removes itself from aggregator when stopped" do
            traverse_ maxOut (allNodesBar p1n2)                          >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                   >>= as' "allow load to disseminate"
            ingestStart    p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncProfileStart                                     >>= as' "wait for async start of profile"
            aggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator created on idle server"
            (traverse_ (aggregatorNotPresent slot1) (allNodesBar p1n2))  >>= as' "aggregator not on busy servers"

            ingestStop     p1n1 slot1 low >>= assertStatusCode 200 >>= as  "stop low ingest"
            waitForAsyncProfileStop                                      >>= as' "wait for async stop of profile"
            aggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                         >>= as  "aggregator has no profiles"

          it "2.5 ingest restarts aggregator if aggregator exits" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
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

          it "2.6 aggregator exits after last profile stops (with linger time)" do
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            ingestStart    p1n1 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest"
            waitForAsyncProfileStart
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as  "aggregator has both profiles"

            ingestStop     p1n1 slot1 low  >>= assertStatusCode 200 >>= as  "stop low ingest"
            waitForAsyncProfileStop
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [high]
                                                                          >>= as  "aggregator only has high"
            ingestStop     p1n1 slot1 high >>= assertStatusCode 200 >>= as  "stop high ingest"
            waitForAsyncProfileStop
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator []
                                                                          >>= as  "aggregator has no profiles"
            waitForMoreThanLinger                                         >>= as' "wait for linger time"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 404 >>= as  "aggregator stops after linger"

          it "2.7 aggregator does not exit during linger time" do
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncProfileStart
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator created"
            ingestStop     p1n1 slot1 low >>= assertStatusCode 200  >>= as  "stop low ingest"
            waitForAsyncProfileStop
            aggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                          >>= as  "aggregator has no profiles"
            waitForLessThanLinger                                         >>= as' "wait for less than the linger time"
            ingestStart    p1n2 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest on another node"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [high]
                                                                          >>= as  "lingered aggregator has high profile"

          it "2.8 aggregator liveness detected on node stop" do
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

          it "2.9 attempt to ingest same profile twice on same node fails" do
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForIntraPoPDisseminate
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator has low profile"
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 500 >>= as  "2nd attempt to create low ingest fails"
            waitForIntraPoPDisseminate
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator has single profile"

          it "2.10 attempt to ingest same profile twice on different node fails" do
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForIntraPoPDisseminate
            intraPoPState p1n1                   >>= assertAggregatorOn [p1n1] slot1
                                                                          >>= as "p1n1 is aware of the ingest on p1n1"
            ingestStart    p1n2 shortName1 low  >>= assertStatusCode 200 >>= as  "2nd attempt to create low ingest succeeds (but actual create is async)"
            intraPoPState p1n1                  >>= assertAggregatorOn [p1n1] slot1
                                                                          >>= as "p1n1 is still aware of the ingest on p1n1"
            waitForIntraPoPDisseminate
            ingestStop     p1n2 slot1 low       >>= assertStatusCode 404  >>= as  "stop ingest fails with 404 since async create failed"
            ingestStop     p1n1 slot1 low       >>= assertStatusCode 200  >>= as  "stop initial ingest"
            waitForIntraPoPDisseminate

            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator []
                                                                          >>= as  "aggregator now has no profiles"

            ingestStart    p1n2 shortName1 low  >>= assertStatusCode 200 >>= as  "final attempt to create low ingest succeeds"
            waitForIntraPoPDisseminate
            aggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator now has low profile again"

    describe "3 Ingest egest tests" do
      describe "3.1 one pop setup"
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
            it "3.1.1 client requests stream on ingest node" do
                clientStart p1n1 slot1           >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
                relayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as  "no relay prior to ingest"
                ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
                waitForAsyncProfileStart                                  >>= as' "wait for async start of profile"
                clientStart p1n1 slot1           >>= assertStatusCode 204 >>= as  "egest available"
                waitForAsyncProfileStart                                  >>= as' "wait for async start of profile"
                relayStats   p1n1 slot1          >>= assertStatusCode 200
                                                     >>= assertRelayForEgest [p1n1]
                                                                          >>= as  "local relay exists"
                egestStats   p1n1 slot1          >>= assertStatusCode 200
                                                     >>= assertEgestClients 1
                                                                          >>= as "agent should have 1 client"
            it "3.1.2 client requests stream on non-ingest node" do
              clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
              relayStats   p1n2 slot1          >>= assertStatusCode 404 >>= as  "no remote relay prior to ingest"
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP source avaialable to disseminate"
              clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "egest available"
              relayStats   p1n2 slot1          >>= assertStatusCode 200 >>= as  "remote relay exists"
              egestStats   p1n2 slot1          >>= assertStatusCode 200
                                                   >>= assertEgestClients 1
                                                                        >>= as "agent should have 1 client"

            it "3.1.3 client requests stream on 2nd node on ingest pop" do
              (flip evalStateT) Map.empty $ do
                lift $ clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as "no egest p1n2 prior to ingest"
                lift $ clientStart p1n3 slot1           >>= assertStatusCode 404 >>= as "no egest p1n3 prior to ingest"
                lift $ ingestStart p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "create ingest"
                lift $ waitForIntraPoPDisseminate                                >>= as' "allow intraPoP source avaialable to disseminate"
                (lift $ clientStart p1n2 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId1"
                                                                                 >>= asT "first egest is same node"
                lift $ waitForIntraPoPDisseminate                                >>= as' "allow intraPoP egest avaialable to disseminate"
                (lift $ clientStart p1n3 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId2"
                                                                                 >>= asT "p1n3 egest redirects to p1n2"
                lift $ egestStats   p1n2 slot1          >>= assertStatusCode 200
                                                        >>= assertEgestClients 2
                                                                                 >>= as "agent should have 2 clients"
                lift $ egestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "no egest on node3"
                (lift $ clientStart p1n2 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId3"
                                                                                 >>= asT "p1n2 stays on node2"
                (lift $ clientStart p1n3 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId4"
                                                                                 >>= asT "p1n3 egest still redirects to p1n2"
                lift $ egestStats   p1n2 slot1          >>= assertStatusCode 200
                                                        >>= assertEgestClients 4
                                                                                 >>= as "agent now has 4 clients"
                lift $ egestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "still no egest on node3"
                clientId1 <- getStateValue "clientId1" "unknown"
                lift $ clientStop clientId1  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 1 on node2"
                clientId2 <- getStateValue "clientId2" "unknown"
                lift $ clientStop clientId2  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 2 on node2"
                clientId3 <- getStateValue "clientId3" "unknown"
                lift $ clientStop clientId3  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 3 on node2"
                clientId4 <- getStateValue "clientId4" "unknown"
                lift $ clientStop clientId4  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 4 on node2"

                lift $ waitForMoreThanEgestLinger                                >>= as' "allow the egest linger timer to expire"
                lift $ egestStats   p1n2 slot1          >>= assertStatusCode 404 >>= as "now no egest on node2"
                lift $ egestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "still no egest on node3"
                lift $ clientStart p1n3 slot1           >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.3")
                                                                                 >>= as "Final egest starts on node3"
                lift $ egestStats   p1n3 slot1          >>= assertStatusCode 200
                                                        >>= assertEgestClients 1
                                                                                 >>= as "node 3 agent should have 1 client"

      describe "3.2 two pop setup" do
        let
          p1Nodes = [p1n1, p1n2, p1n3]
          p2Nodes = [p2n1, p2n2]
          nodes = p1Nodes <> p2Nodes
        before_ (do
                   startSession nodes
                   launch nodes
                ) do
          after_ stopSession do
            it "3.2.1 aggregator presence is disseminated to all servers" do
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200      >>= as  "create ingest"
              waitForTransPoPDisseminate                                    >>= as' "wait for transPop disseminate"
              intraPoPState p1n1                   >>= assertAggregatorOn [p1n1] slot1
                                                                            >>= as "p1n1 is aware of the ingest on p1n1"
              states1 <- traverse forceGetState (Array.toUnfoldable p1Nodes)
              assertSame states1                                            >>= as "All pop 1 nodes agree on leader and aggregator presence"
              states2 <- traverse forceGetState (Array.toUnfoldable p2Nodes)
              assertSame states2                                            >>= as "All pop 2 nodes agree on leader and aggregator presence"

            it "3.2.2 client requests stream on other pop" do
              clientStart p2n1 slot1           >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
              relayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as  "no remote relay prior to ingest"
              relayStats   p2n1 slot1          >>= assertStatusCode 404 >>= as  "no local relay prior to ingest"
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              clientStart p2n1 slot1           >>= assertStatusCode 204 >>= as  "egest available"
              relayStats   p2n1 slot1          >>= assertStatusCode 200 >>= as  "local relay exists"
              waitForAsyncRelayStart                                    >>= as' "wait for the relay chain to start"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP to spread location of relay"
              intraPoPState p1n1               >>= assertStatusCode 200
                                                   >>= assertRelayCount slot1 1
                                                                        >>= as  "relay created in aggregator pop"
              proxiedRelayStats p1n1 slot1     >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p2n1]
                                                                        >>= as  "remote relay is serving local relay"

            it "3.2.3 client ingest starts and stops" do
              clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "no local egest prior to ingest"
              clientStart p2n1 slot1           >>= assertStatusCode 404 >>= as  "no remote egest prior to ingest"
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "local egest post ingest"
              clientStart p2n1 slot1           >>= assertStatusCode 204 >>= as  "remote egest post ingest"
              ingestStop  p1n1 slot1 low >>= assertStatusCode 200 >>= as  "stop the ingest"
              waitForTransPoPStopDisseminate                            >>= as' "wait for transPop disseminate"
              clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "no same pop egest post stop"
              clientStart p2n1 slot1           >>= assertStatusCode 404 >>= as  "no remote pop egest post stop"
              -- TODO - assert the relays stop as well - might be slow with timeouts chaining...


      describe "3.3 node startup - one pop" do
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
            it "3.3.1 a node that starts late gets to see existing streams" do
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "let ingest presence disseminate"
              launch' phase2Nodes sysconfig                             >>= as' "start new node after ingest already running"
              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"
              clientStart p1n3 slot1           >>= assertStatusCode 204 >>= as  "local egest post ingest"

            -- TODO - egest - test stream we think is not present when it is

      describe "3.4 packet loss - one pop" do
        let
          nodes = [p1n1, p1n2]
          sysconfig = "test/config/partial_nodes/sys.config"
        before_ (do
                   startSession nodes
                   launch' nodes sysconfig
                ) do
          after_ stopSession do
            it "3.4.1 aggregator expired after extended packet loss" do
              ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "let ingest presence disseminate"
              clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "local egest post ingest"
              dropAgentMessages p1n2 true                               >>= as  "Drop all agent messages"
              waitForIntraPoPDisseminate                                >>= as' "Wait for less than message expiry"
              clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "Initially clients can still join"
              waitForMessageTimeout                                     >>= as' "Wait for less than message expiry"
              clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "Clients can no longer join"
              dropAgentMessages p1n2 false                              >>= as  "Alow messages to flow once more"

              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"
              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"

              clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "Client can join once more"

      describe "3.5 four pop setup" do
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
            it "3.5.1 lax -> fra sets up 2 non-overlapping relay chains" do
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP to spread location of relay"
              ingestStart p3n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              clientStart p4n1 slot1           >>= assertStatusCode 204 >>= as  "egest available in lax"
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

    describe "4 resilience" do
      let
        p1Nodes = [p1n1, p1n2, p1n3]
        p2Nodes = [p2n1, p2n2]
        p3Nodes = [p3n1]
        nodes = p1Nodes <> p2Nodes <> p3Nodes
        allNodesBar node = delete node nodes
        maxOut server = setLoad server 60.0 >>= assertStatusCode 204 >>= as ("set load on " <> toAddr server)
        sysconfig = "test/config/partial_nodes/sys.config"
      before_ (do
                 startSession nodes
                 launch' nodes sysconfig
              ) do
        after_ stopSession do
          it "4.1 Launch ingest, terminate ingest aggregator process, new ingest aggregator continues to pull from ingest" do
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create ingest"
            waitForAsyncProfileStart                                     >>= as' "wait for async start of profile"
            aggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator has low only"
            killProcess p1n1 (Chaos.defaultKill $ ingestAggregatorName slot1 Primary)
                                                >>=  assertStatusCode 204 >>= as "kill process"
            waitForSupervisorRecovery                                     >>= as' "wait for supervisor"
            aggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator still has low"

          it "4.2 Launch ingest with local aggregator, terminate ingest process, ingest aggregator removes ingest from list of active ingests" do
            ingestStart    p1n1 shortName1 low >>= assertStatusCode 200   >>= as  "create ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            aggregatorStats p1n1 slot1         >>= assertStatusCode 200
                                                   >>= assertAggregator [low]
                                                                          >>= as  "aggregator has low only"
            killProcess p1n1 (Chaos.defaultKill $ ingestName slot1 Primary "500")
                                               >>=  assertStatusCode 204 >>= as "kill process"
            waitForSupervisorRecovery                                    >>= as' "wait for supervisor"
            aggregatorStats p1n1 slot1         >>= assertStatusCode 200
                                                   >>= assertAggregator []
                                                                         >>= as  "aggregator has no ingests"

          it "4.3 Launch ingest with remote aggregator, terminate ingest process, ingest aggregator removes ingest from list of active ingests" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            aggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                          >>= as  "aggregator has low only"
            killProcess p1n1 (Chaos.defaultKill $ ingestName slot1 Primary "500")
                                                >>=  assertStatusCode 204 >>= as "kill process"
            waitForSupervisorRecovery                                     >>= as' "wait for supervisor"
            aggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                          >>= as  "aggregator has no ingests"

          it "4.4 Launch ingest with remote aggregator, terminate ingest node, ingest aggregator removes ingest from list of active ingests" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
            waitForRemoteAsyncProfileStart                                >>= as' "wait for async start of profile"
            aggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator has low only"
            stopNode p1n1                                                >>= as' "stop ingest node"
            waitForIntraPoPDisseminate                                   >>= as' "allow failure to disseminate"
            aggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                         >>= as  "aggregator has no ingests"

          it "4.5 Launch ingest and egest, kill origin relay, assert slot state is still valid" do
            (flip evalStateT) Map.empty $ do
              lift $ ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
              lift $ waitForAsyncProfileStart                                      >>= as' "wait for async start of ingest"
              lift $ clientStart p2n1 slot1              >>= assertStatusCode 204  >>= as  "egest available"
              lift $ waitForAsyncProfileStart                                      >>= as' "wait for async start of egest"
              (lift $ slotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
                                                         <#> ((<$>) canonicaliseSlotState))
                                                         >>= storeSlotState        >>= asT "stored state"
              killOriginRelay slot1 Primary                                        >>= asT' "kill origin relay"
              lift $ waitForAsyncProfileStart                                      >>= as' "wait for recovery"
              (lift $ slotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
                                                         <#> ((<$>) canonicaliseSlotState))
                                                         >>= compareSlotState excludePorts (==)
                                                         >>= compareSlotState identity (/=)
                                                                                   >>= asT "compare state"

          it "4.6 Launch ingest and egest, kill downstream relay, assert slot state is still valid" do
            (flip evalStateT) Map.empty $ do
              lift $ ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
              lift $ waitForAsyncProfileStart                                      >>= as' "wait for async start of ingest"
              lift $ clientStart p2n1 slot1              >>= assertStatusCode 204  >>= as  "egest available"
              lift $ waitForAsyncProfileStart                                      >>= as' "wait for async start of egest"
              (lift $ slotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
                                                         <#> ((<$>) (excludePorts <<< canonicaliseSlotState)))
                                                         >>= storeSlotState        >>= asT "stored state"
              killDownstreamRelay slot1 Primary                                    >>= asT' "kill downstream relay"
              lift $ waitForAsyncProfileStart                                      >>= as' "wait for recovery"
              (lift $ slotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
                                                         <#> ((<$>) canonicaliseSlotState))
                                                         >>= compareSlotState excludePorts (==)
                                                         >>= compareSlotState identity (/=)
                                                                                   >>= asT "compare state"

    describe "Cleanup" do
      after_ stopSession do
        it "final cleanup" do
          pure unit


  where
    testConfig = { slow: Milliseconds 5000.0, timeout: Just (Milliseconds 25000.0), exit: false }

storeSlotState either@(Left _) = pure either
storeSlotState either@(Right slotState) = do
  _ <- modify (Map.insert "slotState" slotState)
  pure either

compareSlotState preFilter predicate either@(Left _) = pure either
compareSlotState preFilter predicate either@(Right slotState) = do
  currentSlotState <- gets (Map.lookup "slotState")
  if
    predicate (Just (preFilter slotState)) (preFilter <$> currentSlotState) then pure either
  else
    let
      _ = spy "lhs" currentSlotState
      _ = spy "rhs" slotState
    in
      pure $ Left "does not match"

killOriginRelay :: SlotId -> SlotRole -> StateT (Map.Map String (PublicState.SlotState Array)) Aff Unit
killOriginRelay slotId slotRole = do
  mCurrentSlotState <- gets (Map.lookup "slotState")
  case mCurrentSlotState of
    Nothing ->
      lift $ throwSlowError $ "No slot state"
    Just {originRelays} ->
      lift $ killRelay slotId slotRole originRelays

killDownstreamRelay :: SlotId -> SlotRole -> StateT (Map.Map String (PublicState.SlotState Array)) Aff Unit
killDownstreamRelay slotId slotRole = do
  mCurrentSlotState <- gets (Map.lookup "slotState")
  case mCurrentSlotState of
    Nothing ->
      lift $ throwSlowError $ "No slot state"
    Just {downstreamRelays} -> do
      lift $ killRelay slotId slotRole downstreamRelays

killRelay :: SlotId -> SlotRole -> Array (JsonLd.StreamRelayStateNode Array) -> Aff Unit
killRelay slotId slotRole relays =
  case firstOf (traversed <<< JsonLd._unwrappedNode <<< JsonLd._id <<< _Just) relays of
      Just id ->
        let
          mServerAddress = urlToServerAddress id
        in
          case mServerAddress of
            Just serverAddress -> do
              _ <- killProcess' (spy "kill" serverAddress) (Chaos.defaultKill $ relayName slotId slotRole)
              pure unit
            Nothing ->
              throwSlowError $ "Failed to parse URL"
      _ ->
        throwSlowError $ "No relays or missing id"

killProcess' :: ServerAddress -> Chaos.ChaosPayload -> Aff (Either String ResponseWithBody)
killProcess' addr chaos =
  fetch (M.URL $ unwrap $ makeUrlAddr addr (Chaos))
               { method: M.postMethod
               , body: SimpleJSON.writeJSON (chaos :: Chaos.ChaosPayload)
               , headers: M.makeHeaders { "Content-Type": "application/json" }
               }

relayName slotId role = Chaos.Gproc (Chaos.GprocTuple2 (Chaos.String "StreamRelay") (Chaos.GprocTuple3 (Chaos.Atom "relayKey") (Chaos.SlotId slotId) (Chaos.SlotRole role)))

urlToServerAddress :: Url -> Maybe ServerAddress
urlToServerAddress url =
  let
    parseOptions = { parseUserInfo: pure
                   , parseHosts: HostPortPair.parser pure pure
                   , parsePath: pure
                   , parseHierPath: pure
                   , parseQuery: pure
                   , parseFragment: pure
                   }
  in
    case runParser (unwrap url) (URIParser.parser parseOptions) of
      Right (URI.URI scheme (HierarchicalPartAuth (Authority _userInfo (Just (Both host _port))) _path) _query _fragment) ->
        Just $ ServerAddress $ URIHost.print host
      _ ->
       Nothing

canonicaliseSlotState :: PublicState.SlotState Array -> PublicState.SlotState Array
canonicaliseSlotState { aggregators
                      , ingests
                      , originRelays
                      , downstreamRelays
                      , egests } =
  { aggregators: sortBy byId aggregators
  , ingests: sortBy byId ingests
  , originRelays: sortBy byId originRelays
  , downstreamRelays: sortBy byId downstreamRelays
  , egests: sortBy byId egests }
  where
    byId :: forall a b. JsonLd.Node a b -> JsonLd.Node a b-> Ordering
    byId (JsonLd.Node {"@id": lhs}) (JsonLd.Node {"@id": rhs}) = compare lhs rhs

excludePorts :: PublicState.SlotState Array -> PublicState.SlotState Array
excludePorts { aggregators
             , ingests
             , originRelays
             , downstreamRelays
             , egests } =
  { aggregators: excludeAggregatorPorts <$> aggregators
  , ingests: ingests
  , originRelays: excludeRelayPorts <$> originRelays
  , downstreamRelays: excludeRelayPorts <$> downstreamRelays
  , egests: egests }
  where
    excludeAggregatorPorts =
      over (JsonLd._unwrappedNode <<< JsonLd._resource <<< JsonLd._downstreamRelays <<< traversed) clearPort
    excludeRelayPorts =
      over (JsonLd._unwrappedNode <<< JsonLd._resource <<< JsonLd._relaysServed <<< traversed) clearPort
    clearPort  =
      set (JsonLd._unwrappedNode <<< JsonLd._resource <<< JsonLd._port) 0

storeHeader :: String -> String -> Either String ResponseWithBody -> StateT (Map.Map String String) Aff (Either String ResponseWithBody)
storeHeader header key either@(Left _) = pure either
storeHeader header key either@(Right {headers}) = do
  let
    value = fromMaybe "unknown" $ Object.lookup header headers
  _ <- modify (Map.insert key value)
  pure either

getStateValue :: forall v m k. MonadState (Map.Map k v) m => Ord k => k -> v -> m v
getStateValue key defaultValue = gets (fromMaybe defaultValue <<< Map.lookup key)

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

assertBodyFun :: forall a. Show a => ReadForeign a => (a -> Boolean) -> Either String ResponseWithBody -> Aff (Either String ResponseWithBody)
assertBodyFun pred left@(Left _) = pure left
assertBodyFun pred (Right rwb) = pure $ (parse rwb) >>= (assertFun'' pred) <#> (const rwb)
  where
    parse :: ReadForeign a => ResponseWithBody -> Either String a
    parse {body} = lmap (jsonError body) $ SimpleJSON.readJSON body
    jsonError body error = "Could not parse json " <> body <> " due to " <> show error

assertFun :: forall a. Show a => (a -> Boolean) -> Either String a -> Aff (Either String a)
assertFun pred either =
  pure $ assertFun' pred either

assertFun' :: forall a. Show a => (a -> Boolean) -> Either String a -> Either String a
assertFun' pred either =
  assertFun'' pred =<< either

assertFun'' :: forall a. Show a => (a -> Boolean) -> a -> Either String a
assertFun'' pred subject =
  if pred subject
  then Right subject
  else Left $ "Predicate failed for content " <> (show subject)

type ToRecord a = Either String ResponseWithBody -> Aff (Either String a)
bodyToRecord ::  forall a. ReadForeign a => ToRecord a
bodyToRecord (Left error) = pure $ Left error
bodyToRecord (Right {body}) =
  pure $ lmap errorText $ SimpleJSON.readJSON body
  where
    errorText error = "Could not parse json " <> body <> " due to " <> show error

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
  delay (Milliseconds 1000.0)
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
