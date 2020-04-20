module Main where

import Prelude

import Data.Array (delete, length, sort, sortBy)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum)
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Lens (_Just, firstOf, over, set, traversed)
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (un, unwrap)
import Data.String as String
import Data.Time (Time(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, throwError)
import Effect.Exception (error) as Exception
import Effect.Unsafe (unsafePerformEffect)
import Effect.Now as Now
import Foreign.Object as Object
import Milkis as M
import OsCmd (runProc)
import Partial.Unsafe (unsafePartial)
import Shared.Chaos as Chaos
import Shared.Stream (SlotId, SlotNameAndProfileName(..), SlotRole(..))
import Shared.Router.Endpoint (Canary(..), Endpoint(..), makeUrlAddr)
import Shared.Common (Url)
import Shared.Types (ServerAddress(..), extractAddress)
import Shared.Types.Agent.State as PublicState
import Shared.Rtsv2.JsonLd as JsonLd
import Simple.JSON (class ReadForeign)
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
import Control.Monad.State (StateT, evalStateT, lift)
import Control.Monad.State.Class (class MonadState, gets, modify)
import Helpers.Env as Env
import Helpers.HTTP as HTTP
import Helpers.Types (Node(..), TestNode)
import Helpers.Node (startSession, stopSession, stopNode)
import Helpers.Env
import Helpers.CreateString (toAddrFromNode, mkServerAddress, makeUrlAndUnwrap)
import Helpers.Log (throwSlowError, maybeLogStep, as, as', asT, asT')


forceRight :: forall a b. Either a b -> b
forceRight e = unsafePartial $ case e of
  Right b -> b

main :: Effect Unit
main =
  let

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
          (sort $ (ServerAddress <<< toAddrFromNode) <$> servers) == sort (_.address <<< JsonLd.unwrapNode <$> (JsonLd.unwrapNode streamRelayState).egestsServed)

    assertRelayForRelay = assertBodyFun <<< predicate
      where
        predicate :: Array Node -> PublicState.StreamRelay Array -> Boolean
        predicate servers streamRelayState =
          (sort $ (ServerAddress <<< toAddrFromNode) <$> servers) == sort (_.address <<< unwrap <<< _.server <<< JsonLd.unwrapNode <$> (JsonLd.unwrapNode streamRelayState).relaysServed)

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
            nodeAddresses = toAddrFromNode
            serverAddressesForSlotId = foldl (\acc {slotId, servers} ->
                                                 if slotId == requiredSlotId
                                                 then acc <> (extractAddress <<< JsonLd.unwrapNode <$> servers)
                                                 else acc
                                               ) []  (JsonLd.unwrapNode popState).aggregatorLocations
          in
          (sort $ (ServerAddress <<< toAddrFromNode) <$> nodes) == sort serverAddressesForSlotId

    assertRelayCount requiredSlotId count = assertBodyFun $ predicate
      where
        predicate :: (PublicState.IntraPoP Array) -> Boolean
        predicate popState =
          let
            nodeAddresses = toAddrFromNode
            serverAddressesForSlotId = foldl (\acc {slotId, servers} ->
                                                 if slotId == requiredSlotId
                                                 then acc <> (extractAddress <<< JsonLd.unwrapNode <$> servers)
                                                 else acc
                                               ) []  (JsonLd.unwrapNode popState).relayLocations
          in
          length (sort serverAddressesForSlotId) == count

    forceGetState :: Node -> Aff (JsonLd.IntraPoPState Array)
    forceGetState node = JsonLd.unwrapNode <$> forceRight <$> (jsonToType' :: HTTP.ResWithBody -> Either String (PublicState.IntraPoP Array)) <$> forceRight <$> HTTP.getIntraPoPState node

    assertSame :: forall a. Show a => Eq a => List a -> Aff (Either String Unit)
    assertSame Nil = pure $ Right unit
    assertSame (Cons x Nil) = pure $ Right unit
    assertSame (Cons x t@(Cons y xs)) = if x == y then assertSame t
                                        else pure $ Left $ "\"" <> (show x) <> "\" is not the same as \"" <> (show y) <> "\""

    jsonToType' :: forall a. ReadForeign a => HTTP.ResWithBody -> Either String a
    jsonToType' {body} = lmap (const "Failed to parse JSON") $ SimpleJSON.readJSON body

    assertBodiesSame :: List HTTP.ResWithBody -> Aff (Either String Unit)
    assertBodiesSame Nil = pure $ Right unit
    assertBodiesSame (Cons x xs) = pure $ assertBodiesSame_ x.body xs
      where
        assertBodiesSame_ :: String -> List HTTP.ResWithBody -> Either String Unit
        assertBodiesSame_ _ Nil = Right unit
        assertBodiesSame_ firstBody (Cons y ys) =
          if y.body == firstBody
          then assertBodiesSame_ firstBody ys
          else Left $ "\"" <> firstBody <> "\" is not the same as \"" <> y.body <> "\""

  in
  launchAff_ $ un Identity $ runSpecT testConfig [consoleReporter] do
    describe "1 Startup tests"
      let
        p1Nodes = [Env.p1n1, Env.p1n2, Env.p1n3]
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
        p1Nodes = [Env.p1n1, Env.p1n2, Env.p1n3]
        nodes = p1Nodes
        allNodesBar node = delete node nodes
        maxOut server = HTTP.setLoad server 60.0 >>= assertStatusCode 204 >>= as ("set load on " <> toAddrFromNode server)
        aggregatorNotPresent slot server = HTTP.getAggregatorStats server slot >>= assertStatusCode 404 >>= as ("aggregator not on " <> toAddrFromNode server)
      in do
      before_ (do
                 startSession nodes
                 launch nodes
              ) do
        after_ stopSession do
          it "2.1 ingest aggregation created on ingest node" do
            HTTP.ingestStart    p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
            waitForAsyncProfileStart                                     >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator has low only"

          it "2.2 2nd ingest doesn't start new aggregator since one is running" do
            HTTP.ingestStart    p1n1 shortName1 low   >>= assertStatusCode 200 >>= as "create low ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            HTTP.setLoad         p1n1 60.0            >>= assertStatusCode 204 >>= as "set load on server"
            HTTP.ingestStart    p1n1 shortName1 high  >>= assertStatusCode 200 >>= as "create high ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as "aggregator has 2 profiles"

          it "2.3 if ingest node is too loaded, then ingest aggregation starts on non-ingest node" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForIntraPoPDisseminate                                    >>= as' "allow remote ingest location to disseminate"
            HTTP.ingestStart    p1n1 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n2 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as  "aggregator is on p1n2"

          it "2.4 ingest on different node removes itself from aggregator when stopped" do
            traverse_ maxOut (allNodesBar p1n2)                          >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                   >>= as' "allow load to disseminate"
            HTTP.ingestStart    p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncProfileStart                                     >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator created on idle server"
            (traverse_ (aggregatorNotPresent slot1) (allNodesBar p1n2))  >>= as' "aggregator not on busy servers"

            HTTP.ingestStop     p1n1 slot1 low >>= assertStatusCode 200 >>= as  "stop low ingest"
            waitForAsyncProfileStop                                      >>= as' "wait for async stop of profile"
            HTTP.getAggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                         >>= as  "aggregator has no profiles"

          it "2.5 ingest restarts aggregator if aggregator exits" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n2 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator created on idle server"
            traverse_ (aggregatorNotPresent slot1) (allNodesBar p1n2)     >>= as' "aggregator not on busy servers"
            HTTP.setLoad         p1n3 0.0             >>= assertStatusCode 204 >>= as  "mark p1n3 as idle"
            stopNode p1n2                        >>= as' "make p1n2 fail"
            waitForNodeFailureDisseminate                                 >>= as' "allow failure to disseminate"
            HTTP.getAggregatorStats p1n3 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "failed aggregator moved to new idle server"

          it "2.6 aggregator exits after last profile stops (with linger time)" do
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            HTTP.ingestStart    p1n1 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest"
            waitForAsyncProfileStart
            HTTP.getAggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low, high]
                                                                          >>= as  "aggregator has both profiles"

            HTTP.ingestStop     p1n1 slot1 low  >>= assertStatusCode 200 >>= as  "stop low ingest"
            waitForAsyncProfileStop
            HTTP.getAggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [high]
                                                                          >>= as  "aggregator only has high"
            HTTP.ingestStop     p1n1 slot1 high >>= assertStatusCode 200 >>= as  "stop high ingest"
            waitForAsyncProfileStop
            HTTP.getAggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator []
                                                                          >>= as  "aggregator has no profiles"
            waitForMoreThanLinger                                         >>= as' "wait for linger time"
            HTTP.getAggregatorStats p1n1 slot1           >>= assertStatusCode 404 >>= as  "aggregator stops after linger"

          it "2.7 aggregator does not exit during linger time" do
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForAsyncProfileStart
            HTTP.getAggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator created"
            HTTP.ingestStop     p1n1 slot1 low >>= assertStatusCode 200  >>= as  "stop low ingest"
            waitForAsyncProfileStop
            HTTP.getAggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                          >>= as  "aggregator has no profiles"
            waitForLessThanLinger                                         >>= as' "wait for less than the linger time"
            HTTP.ingestStart    p1n2 shortName1 high >>= assertStatusCode 200 >>= as  "create high ingest on another node"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [high]
                                                                          >>= as  "lingered aggregator has high profile"

          it "2.8 aggregator liveness detected on node stop" do
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForIntraPoPDisseminate
            HTTP.getIntraPoPState p1n1                   >>= assertAggregatorOn [p1n1] slot1
                                                                          >>= as "p1n1 is aware of the ingest on p1n1"
            HTTP.getIntraPoPState p1n2                   >>= assertAggregatorOn [p1n1] slot1
                                                                          >>= as "p1n2 is aware of the ingest on p1n1"
            stopNode p1n1                        >>= as' "make p1n1 fail"
            waitForNodeFailureDisseminate                                 >>= as' "allow failure to disseminate"
            HTTP.getIntraPoPState p1n2                   >>= assertAggregatorOn [] slot1
                                                                          >>= as "p1n2 is aware the ingest stopped"

          it "2.9 attempt to ingest same profile twice on same node fails" do
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForIntraPoPDisseminate
            HTTP.getAggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator has low profile"
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 500 >>= as  "2nd attempt to create low ingest fails"
            waitForIntraPoPDisseminate
            HTTP.getAggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator [low]
                                                                          >>= as  "aggregator has single profile"

          it "2.10 attempt to ingest same profile twice on different node fails" do
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create low ingest"
            waitForIntraPoPDisseminate
            HTTP.getIntraPoPState p1n1                   >>= assertAggregatorOn [p1n1] slot1
                                                                          >>= as "p1n1 is aware of the ingest on p1n1"
            HTTP.ingestStart    p1n2 shortName1 low  >>= assertStatusCode 200 >>= as  "2nd attempt to create low ingest succeeds (but actual create is async)"
            HTTP.getIntraPoPState p1n1                  >>= assertAggregatorOn [p1n1] slot1
                                                                          >>= as "p1n1 is still aware of the ingest on p1n1"
            waitForIntraPoPDisseminate
            HTTP.ingestStop     p1n2 slot1 low       >>= assertStatusCode 404  >>= as  "stop ingest fails with 404 since async create failed"
            HTTP.ingestStop     p1n1 slot1 low       >>= assertStatusCode 200  >>= as  "stop initial ingest"
            waitForAsyncProfileStart

            HTTP.getAggregatorStats p1n1 slot1           >>= assertStatusCode 200
                                                     >>= assertAggregator []
                                                                          >>= as  "aggregator now has no profiles"

            HTTP.ingestStart    p1n2 shortName1 low  >>= assertStatusCode 200 >>= as  "final attempt to create low ingest succeeds"
            waitForIntraPoPDisseminate
            HTTP.getAggregatorStats p1n1 slot1           >>= assertStatusCode 200
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
                HTTP.clientStart p1n1 slot1           >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
                HTTP.getRelayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as  "no relay prior to ingest"
                HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
                waitForAsyncProfileStart                                  >>= as' "wait for async start of profile"
                HTTP.clientStart p1n1 slot1           >>= assertStatusCode 204 >>= as  "egest available"
                waitForAsyncProfileStart                                  >>= as' "wait for async start of profile"
                HTTP.getRelayStats   p1n1 slot1          >>= assertStatusCode 200
                                                     >>= assertRelayForEgest [p1n1]
                                                                          >>= as  "local relay exists"
                HTTP.getEgestStats   p1n1 slot1          >>= assertStatusCode 200
                                                     >>= assertEgestClients 1
                                                                          >>= as "agent should have 1 client"
            it "3.1.2 client requests stream on non-ingest node" do
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
              HTTP.getRelayStats   p1n2 slot1          >>= assertStatusCode 404 >>= as  "no remote relay prior to ingest"
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP source avaialable to disseminate"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "egest available"
              HTTP.getRelayStats   p1n2 slot1          >>= assertStatusCode 200 >>= as  "remote relay exists"
              HTTP.getEgestStats   p1n2 slot1          >>= assertStatusCode 200
                                                   >>= assertEgestClients 1
                                                                        >>= as "agent should have 1 client"

            it "3.1.3 client requests stream on 2nd node on ingest pop" do
              (flip evalStateT) Map.empty $ do
                lift $ HTTP.clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as "no egest p1n2 prior to ingest"
                lift $ HTTP.clientStart p1n3 slot1           >>= assertStatusCode 404 >>= as "no egest p1n3 prior to ingest"
                lift $ HTTP.ingestStart p1n1 shortName1 low  >>= assertStatusCode 200 >>= as "create ingest"
                lift $ waitForIntraPoPDisseminate                                >>= as' "allow intraPoP source avaialable to disseminate"
                (lift $ HTTP.clientStart p1n2 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId1"
                                                                                 >>= asT "first egest is same node"
                lift $ waitForIntraPoPDisseminate                                >>= as' "allow intraPoP egest avaialable to disseminate"
                (lift $ HTTP.clientStart p1n3 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId2"
                                                                                 >>= asT "p1n3 egest redirects to p1n2"
                lift $ HTTP.getEgestStats   p1n2 slot1          >>= assertStatusCode 200
                                                        >>= assertEgestClients 2
                                                                                 >>= as "agent should have 2 clients"
                lift $ HTTP.getEgestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "no egest on node3"
                (lift $ HTTP.clientStart p1n2 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId3"
                                                                                 >>= asT "p1n2 stays on node2"
                (lift $ HTTP.clientStart p1n3 slot1          >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.2"))
                                                        >>= storeHeader "x-client-id" "clientId4"
                                                                                 >>= asT "p1n3 egest still redirects to p1n2"
                lift $ HTTP.getEgestStats   p1n2 slot1          >>= assertStatusCode 200
                                                        >>= assertEgestClients 4
                                                                                 >>= as "agent now has 4 clients"
                lift $ HTTP.getEgestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "still no egest on node3"
                clientId1 <- getStateValue "clientId1" "unknown"
                lift $ HTTP.clientStop clientId1  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 1 on node2"
                clientId2 <- getStateValue "clientId2" "unknown"
                lift $ HTTP.clientStop clientId2  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 2 on node2"
                clientId3 <- getStateValue "clientId3" "unknown"
                lift $ HTTP.clientStop clientId3  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 3 on node2"
                clientId4 <- getStateValue "clientId4" "unknown"
                lift $ HTTP.clientStop clientId4  p1n2 slot1 >>= assertStatusCode 204 >>= as "stop client 4 on node2"

                lift $ waitForMoreThanEgestLinger                                >>= as' "allow the egest linger timer to expire"
                lift $ HTTP.getEgestStats   p1n2 slot1          >>= assertStatusCode 404 >>= as "now no egest on node2"
                lift $ HTTP.getEgestStats   p1n3 slot1          >>= assertStatusCode 404 >>= as "still no egest on node3"
                lift $ HTTP.clientStart p1n3 slot1           >>= assertStatusCode 204
                                                        >>= assertHeader (Tuple "x-servedby" "172.16.169.3")
                                                                                 >>= as "Final egest starts on node3"
                lift $ HTTP.getEgestStats   p1n3 slot1          >>= assertStatusCode 200
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
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200      >>= as  "create ingest"
              waitForTransPoPDisseminate                                    >>= as' "wait for transPop disseminate"
              HTTP.getIntraPoPState p1n1                   >>= assertAggregatorOn [p1n1] slot1
                                                                            >>= as "p1n1 is aware of the ingest on p1n1"
              states1 <- traverse forceGetState (Array.toUnfoldable p1Nodes)
              assertSame states1                                            >>= as "All pop 1 nodes agree on leader and aggregator presence"
              states2 <- traverse forceGetState (Array.toUnfoldable p2Nodes)
              assertSame states2                                            >>= as "All pop 2 nodes agree on leader and aggregator presence"

            it "3.2.2 client requests stream on other pop" do
              HTTP.clientStart p2n1 slot1           >>= assertStatusCode 404 >>= as  "no egest prior to ingest"
              HTTP.getRelayStats   p1n1 slot1          >>= assertStatusCode 404 >>= as  "no remote relay prior to ingest"
              HTTP.getRelayStats   p2n1 slot1          >>= assertStatusCode 404 >>= as  "no local relay prior to ingest"
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              HTTP.clientStart p2n1 slot1           >>= assertStatusCode 204 >>= as  "egest available"
              HTTP.getRelayStats   p2n1 slot1          >>= assertStatusCode 200 >>= as  "local relay exists"
              waitForAsyncRelayStart                                    >>= as' "wait for the relay chain to start"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP to spread location of relay"
              HTTP.getIntraPoPState p1n1               >>= assertStatusCode 200
                                                   >>= assertRelayCount slot1 1
                                                                        >>= as  "relay created in aggregator pop"
              HTTP.getProxiedRelayStats p1n1 slot1     >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p2n1]
                                                                        >>= as  "remote relay is serving local relay"

            it "3.2.3 client ingest starts and stops" do
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "no local egest prior to ingest"
              HTTP.clientStart p2n1 slot1           >>= assertStatusCode 404 >>= as  "no remote egest prior to ingest"
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "local egest post ingest"
              HTTP.clientStart p2n1 slot1           >>= assertStatusCode 204 >>= as  "remote egest post ingest"
              HTTP.ingestStop  p1n1 slot1 low >>= assertStatusCode 200 >>= as  "stop the ingest"
              waitForTransPoPStopDisseminate                            >>= as' "wait for transPop disseminate"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "no same pop egest post stop"
              HTTP.clientStart p2n1 slot1           >>= assertStatusCode 404 >>= as  "no remote pop egest post stop"
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
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "let ingest presence disseminate"
              launch' phase2Nodes sysconfig                             >>= as' "start new node after ingest already running"
              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"
              HTTP.clientStart p1n3 slot1           >>= assertStatusCode 204 >>= as  "local egest post ingest"

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
              HTTP.ingestStart p1n1 shortName1 low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForIntraPoPDisseminate                                >>= as' "let ingest presence disseminate"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "local egest post ingest"
              HTTP.dropAgentMessages p1n2 true                               >>= as  "Drop all agent messages"
              waitForIntraPoPDisseminate                                >>= as' "Wait for less than message expiry"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "Initially clients can still join"
              waitForMessageTimeout                                     >>= as' "Wait for less than message expiry"
              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 404 >>= as  "Clients can no longer join"
              HTTP.dropAgentMessages p1n2 false                              >>= as  "Alow messages to flow once more"

              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"
              waitForNodeStartDisseminate                               >>= as' "let ingest presence disseminate"

              HTTP.clientStart p1n2 slot1           >>= assertStatusCode 204 >>= as  "Client can join once more"

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
              HTTP.ingestStart p3n1 shortName1  low >>= assertStatusCode 200 >>= as  "create ingest"
              waitForTransPoPDisseminate                                >>= as' "wait for transPop disseminate"
              HTTP.clientStart p4n1 slot1           >>= assertStatusCode 204 >>= as  "egest available in lax"
              HTTP.getRelayStats  p4n1 slot1           >>= assertStatusCode 200 >>= as  "local relay exists"
              waitForAsyncRelayStart                                    >>= as' "wait for the relay chain to start"
              waitForIntraPoPDisseminate                                >>= as' "allow intraPoP to spread location of relay"
              HTTP.getRelayStats p1n1 slot1            >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p4n1]
                                                       >>= assertRelayForEgest []
                                                                        >>= as  "iad relays for lax with no egests of its own"

              HTTP.getRelayStats p2n1 slot1            >>= assertStatusCode 200
                                                   >>= assertRelayForRelay [p4n1]
                                                       >>= assertRelayForEgest []
                                                                        >>= as  "dal relays for lax with no egests of its own"
              HTTP.getRelayStats p3n1 slot1            >>= assertStatusCode 200
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
        maxOut server = HTTP.setLoad server 60.0 >>= assertStatusCode 204 >>= as ("set load on " <> toAddrFromNode server)
        sysconfig = "test/config/partial_nodes/sys.config"
      before_ (do
                 startSession nodes
                 launch' nodes sysconfig
              ) do
        after_ stopSession do
          it "4.1 Launch ingest, terminate ingest aggregator process, new ingest aggregator continues to pull from ingest" do
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200 >>= as  "create ingest"
            waitForAsyncProfileStart                                     >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator has low only"
            HTTP.killProcessNode p1n1 (Chaos.defaultKill $ ingestAggregatorName slot1 Primary)
                                                >>=  assertStatusCode 204 >>= as "kill process"
            waitForSupervisorRecovery                                     >>= as' "wait for supervisor"
            HTTP.getAggregatorStats p1n1 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator still has low"

          it "4.2 Launch ingest with local aggregator, terminate ingest process, ingest aggregator removes ingest from list of active ingests" do
            HTTP.ingestStart    p1n1 shortName1 low >>= assertStatusCode 200   >>= as  "create ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n1 slot1         >>= assertStatusCode 200
                                                   >>= assertAggregator [low]
                                                                          >>= as  "aggregator has low only"
            HTTP.killProcessNode p1n1 (Chaos.defaultKill $ ingestName slot1 Primary "500")
                                               >>=  assertStatusCode 204 >>= as "kill process"
            waitForSupervisorRecovery                                    >>= as' "wait for supervisor"
            HTTP.getAggregatorStats p1n1 slot1         >>= assertStatusCode 200
                                                   >>= assertAggregator []
                                                                         >>= as  "aggregator has no ingests"

          it "4.3 Launch ingest with remote aggregator, terminate ingest process, ingest aggregator removes ingest from list of active ingests" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                          >>= as  "aggregator has low only"
            HTTP.killProcessNode p1n1 (Chaos.defaultKill $ ingestName slot1 Primary "500")
                                                >>=  assertStatusCode 204 >>= as "kill process"
            waitForSupervisorRecovery                                     >>= as' "wait for supervisor"
            HTTP.getAggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                          >>= as  "aggregator has no ingests"

          it "4.4 Launch ingest with remote aggregator, terminate ingest node, ingest aggregator removes ingest from list of active ingests" do
            traverse_ maxOut (allNodesBar p1n2)                           >>= as' "load up all servers bar one"
            waitForIntraPoPDisseminate                                    >>= as' "allow load to disseminate"
            HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
            waitForAsyncProfileStart                                      >>= as' "wait for async start of profile"
            HTTP.getAggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator [low]
                                                                         >>= as  "aggregator has low only"
            stopNode p1n1                                                >>= as' "stop ingest node" -- no sleep needed, since stopNode takes time...
            HTTP.getAggregatorStats p1n2 slot1          >>= assertStatusCode 200
                                                    >>= assertAggregator []
                                                                         >>= as  "aggregator has no ingests"

          it "4.5 Launch ingest and egest, kill origin relay, assert slot state is still valid" do
            (flip evalStateT) Map.empty $ do
              lift $ HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
              lift $ waitForTransPoPDisseminate                                    >>= as' "wait for async start of ingest"
              lift $ HTTP.clientStart p2n1 slot1              >>= assertStatusCode 204  >>= as  "egest available"
              lift $ waitForTransPoPDisseminate                                    >>= as' "wait for async start of egest"
              (lift $ HTTP.getSlotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
                                                         <#> ((<$>) canonicaliseSlotState))
                                                         >>= storeSlotState        >>= asT "stored state"
              killOriginRelay slot1 Primary                                        >>= asT' "kill origin relay"
              lift $ waitForAsyncProfileStart                                      >>= as' "wait for recovery"
              (lift $ HTTP.getSlotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
                                                         <#> ((<$>) canonicaliseSlotState))
                                                         >>= compareSlotState excludePorts (==)
                                                         >>= compareSlotState identity (/=)
                                                                                   >>= asT "compare state"

          it "4.6 Launch ingest and egest, kill downstream relay, assert slot state is still valid" do
            (flip evalStateT) Map.empty $ do
              lift $ HTTP.ingestStart    p1n1 shortName1 low  >>= assertStatusCode 200  >>= as  "create ingest"
              lift $ waitForTransPoPDisseminate                                    >>= as' "wait for async start of ingest"
              lift $ HTTP.clientStart p2n1 slot1              >>= assertStatusCode 204  >>= as  "egest available"
              lift $ waitForTransPoPDisseminate                                    >>= as' "wait for async start of egest"
              (lift $ HTTP.getSlotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
                                                         <#> ((<$>) (excludePorts <<< canonicaliseSlotState)))
                                                         >>= storeSlotState        >>= asT "stored state"
              killDownstreamRelay slot1 Primary                                    >>= asT' "kill downstream relay"
              lift $ waitForAsyncProfileStart                                      >>= as' "wait for recovery"
              (lift $ HTTP.getSlotState p1n1 slot1               >>= (bodyToRecord :: ToRecord (PublicState.SlotState Array))
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
              _ <- HTTP.killProcessServerAddr (spy "kill" serverAddress) (Chaos.defaultKill $ relayName slotId slotRole)
              pure unit
            Nothing ->
              throwSlowError $ "Failed to parse URL"
      _ ->
        throwSlowError $ "No relays or missing id"

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

storeHeader :: String -> String -> Either String HTTP.ResWithBody -> StateT (Map.Map String String) Aff (Either String HTTP.ResWithBody)
storeHeader header key either@(Left _) = pure either
storeHeader header key either@(Right {headers}) = do
  let
    value = fromMaybe "unknown" $ Object.lookup header headers
  _ <- modify (Map.insert key value)
  pure either

getStateValue :: forall v m k. MonadState (Map.Map k v) m => Ord k => k -> v -> m v
getStateValue key defaultValue = gets (fromMaybe defaultValue <<< Map.lookup key)

assertStatusCode :: Int -> Either String HTTP.ResWithBody -> Aff (Either String HTTP.ResWithBody)
assertStatusCode expectedCode either =
  pure $ either >>= (\rwb ->
                      if rwb.statusCode == expectedCode
                      then either
                      else Left $  "Unexpected statuscode: Expected " <> show expectedCode <> ", got " <> show rwb.statusCode
                    )

assertHeader :: Tuple String String -> Either String HTTP.ResWithBody -> Aff (Either String HTTP.ResWithBody)
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

assertBodyFun :: forall a. Show a => ReadForeign a => (a -> Boolean) -> Either String HTTP.ResWithBody -> Aff (Either String HTTP.ResWithBody)
assertBodyFun pred left@(Left _) = pure left
assertBodyFun pred (Right rwb) = pure $ (parse rwb) >>= (assertFun'' pred) <#> (const rwb)
  where
    parse :: ReadForeign a => HTTP.ResWithBody -> Either String a
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

type ToRecord a = Either String HTTP.ResWithBody -> Aff (Either String a)
bodyToRecord ::  forall a. ReadForeign a => ToRecord a
bodyToRecord (Left error) = pure $ Left error
bodyToRecord (Right {body}) =
  pure $ lmap errorText $ SimpleJSON.readJSON body
  where
    errorText error = "Could not parse json " <> body <> " due to " <> show error


toIfaceIndexString :: Node -> String
toIfaceIndexString (Node popNum nodeNum) = show (popNum * 10) <> show nodeNum

mkNode :: String  -> Node -> TestNode
mkNode sysConfig node = {ifaceIndexString: toIfaceIndexString node, addr: toAddrFromNode node, sysConfig}





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
