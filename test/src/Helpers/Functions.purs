module Helpers.Functions where

import Prelude

import Control.Monad.State (StateT, lift)
import Control.Monad.State.Class (class MonadState, gets, modify)
import Data.Array (sortBy, delete)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Lens (_Just, firstOf, over, set, traversed)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap, unwrap)
import Data.These (These(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
import Debug.Trace (spy)
import Effect.Aff (Aff, delay)
import Foreign (unsafeFromForeign)
import Foreign.Object as Object
import Helpers.Assert as A
import Helpers.CreateString (mkPoPJsonString, toAddrFromNode, toIfaceIndexString, mkServerAddress)
import Helpers.Env (sessionName)
import Helpers.HTTP as HTTP
import Helpers.Log (throwSlowError, as)
import Helpers.OsCmd (runProc)
import Helpers.Types (Node, PoPInfo, ResWithBody, TestNode, ToRecord)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Partial.Unsafe (unsafePartial)
import Shared.Common (Url)
import Shared.Rtsv2.Agent.State as PublicState
import Shared.Rtsv2.Chaos as Chaos
import Shared.Rtsv2.JsonLd as JsonLd
import Shared.Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Rtsv2.Stream (RtmpShortName(..), RtmpStreamName(..), SlotId, SlotRole)
import Shared.Rtsv2.Types (ServerAddress(..))
import Simple.JSON (class ReadForeign)
import Simple.JSON as SimpleJSON
import Text.Parsing.Parser (runParser)
import Toppokki as T
import URI as URI
import URI.Authority (Authority(..))
import URI.HierarchicalPart (HierarchicalPart(..))
import URI.Host as URIHost
import URI.HostPortPair as HostPortPair
import URI.URI as URIParser


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

-------------------------------------------------------------------------------
-- Browser
-------------------------------------------------------------------------------
getInnerText :: String -> T.Page -> Aff String
getInnerText selector page = do
  innerTextF <- T.unsafePageEval
                (T.Selector selector)
                "el => el.innerText"
                page
  let innerText = (unsafeFromForeign innerTextF) :: String
  pure innerText

stringToInt :: String -> Int
stringToInt s =
  fromMaybe 0 $ fromString s

mkPlayerUrl :: Node -> SlotId -> SlotRole -> T.URL
mkPlayerUrl node slotId slotRole =
  T.URL $ unwrap $ makeUrl (mkServerAddress node) $ ClientPlayerE slotId slotRole

mkIngestUrl :: Node -> RtmpShortName -> RtmpStreamName -> T.URL
mkIngestUrl node shortName streamName =
  T.URL $ unwrap $ makeUrl (mkServerAddress node) $ ClientWebRTCIngestE shortName streamName

-------------------------------------------------------------------------------
-- Node
-------------------------------------------------------------------------------
mkNode :: String  -> Node -> TestNode
mkNode sysConfig node =
  {ifaceIndexString: toIfaceIndexString node, addr: toAddrFromNode node, sysConfig}

stopNode :: Node -> Aff Unit
stopNode node = do
  let nodeAddr = toAddrFromNode node
  runProc "./scripts/stopNode.sh" [ sessionName
                                  , nodeAddr
                                  , nodeAddr
                                  ]

startSession :: Array Node -> Aff Unit
startSession allNodes = do
  stopSession
  writeTextFile UTF8 "config/popDefinition.json" $ mkPoPJsonString allNodes
  runProc "./scripts/startSession.sh" [sessionName]

stopSession :: Aff Unit
stopSession = do
  _ <- delay (Milliseconds 200.0)
  runProc "./scripts/stopSession.sh" [sessionName]

startSlotHigh1000 :: String -> Aff Unit
startSlotHigh1000 ip = do
  _ <- delay (Milliseconds 8000.0)
  runProc "./scripts/run_slot1_1000.sh" [ip]

start2SlotHigh1000 :: String -> Aff Unit
start2SlotHigh1000 ip = do
  _ <- delay (Milliseconds 8000.0)
  runProc "./scripts/run2_slot1_1000.sh" [ip]

startSlotHigh1000Backup :: String -> Aff Unit
startSlotHigh1000Backup ip = do
  _ <- delay (Milliseconds 8000.0)
  runProc "./scripts/run_slot1b_1000.sh" [ip]

startSlotHigh1000Canary :: String -> Aff Unit
startSlotHigh1000Canary ip = do
  _ <- delay (Milliseconds 8000.0)
  runProc "./scripts/run_slot1_1000.sh" [ip, "1936"]

stopSlot :: Aff Unit
stopSlot = do
  runProc "./scripts/stopSlot.sh" []

-------------------------------------------------------------------------------
-- Relay
-------------------------------------------------------------------------------
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

relayName :: SlotId -> SlotRole -> Chaos.ChaosName
relayName slotId role =
  Chaos.Gproc (Chaos.GprocTuple2
                (Chaos.String "StreamRelay")
                (Chaos.GprocTuple3
                   (Chaos.Atom "relayKey")
                   (Chaos.SlotId slotId)
                   (Chaos.SlotRole role)))


-------------------------------------------------------------------------------
-- PoP
-------------------------------------------------------------------------------
makePoPInfo :: String -> Int -> PoPInfo
makePoPInfo n i = {name: n, number: i, x: 0.0, y: 0.0}

-------------------------------------------------------------------------------
-- Slot
-------------------------------------------------------------------------------
storeSlotState either@(Left _) = pure either
storeSlotState either@(Right slotState) = do
  _ <- modify (Map.insert "slotState" slotState)
  pure either

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

excludeTimestamps :: PublicState.SlotState Array -> PublicState.SlotState Array
excludeTimestamps state@{ egests } =
  state{egests = excludeEgestTimestamps <$> egests}
  where
    excludeEgestTimestamps =
      over (JsonLd._unwrappedNode <<< JsonLd._resource) clearTimestamp
    clearTimestamp =
      set JsonLd._timestamp (wrap 0.0)

-------------------------------------------------------------------------------
-- Others
-------------------------------------------------------------------------------
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

forceRight :: forall a b. Either a b -> b
forceRight e = unsafePartial $ case e of
  Right b -> b

storeHeader :: String -> String -> Either String ResWithBody -> StateT (Map.Map String String) Aff (Either String ResWithBody)
storeHeader header key either@(Left _) = pure either
storeHeader header key either@(Right {headers}) = do
  let
    value = fromMaybe "unknown" $ Object.lookup header headers
  _ <- modify (Map.insert key value)
  pure either


bodyToRecord ::  forall a. ReadForeign a => ToRecord a
bodyToRecord (Left error) = pure $ Left error
bodyToRecord (Right {body}) =
  pure $ lmap errorText $ SimpleJSON.readJSON body
  where
    errorText error = "Could not parse json " <> body <> " due to " <> show error


getStateValue :: forall v m k. MonadState (Map.Map k v) m => Ord k => k -> v -> m v
getStateValue key defaultValue = gets (fromMaybe defaultValue <<< Map.lookup key)

jsonToType' :: forall a. ReadForeign a => ResWithBody -> Either String a
jsonToType' {body} = lmap (const "Failed to parse JSON") $ SimpleJSON.readJSON body

forceGetState :: Node -> Aff (JsonLd.IntraPoPState Array)
forceGetState node =
  JsonLd.unwrapNode <$> forceRight
                    <$> (jsonToType' :: ResWithBody -> Either String (PublicState.IntraPoP Array))
                    <$> forceRight
                    <$> HTTP.getIntraPoPState node

allNodesBar :: Node -> Array Node -> Array Node
allNodesBar node nodes = delete node nodes

maxOut :: Node -> Aff Unit
maxOut server = HTTP.setLoad server 91.0 >>= A.assertStatusCode 204 >>= as ("set load on " <> toAddrFromNode server)

aggregatorNotPresent :: SlotId -> Node -> Aff Unit
aggregatorNotPresent slot server = HTTP.getAggregatorStats server slot >>= A.assertStatusCode 404 >>= as ("aggregator not on " <> toAddrFromNode server)
