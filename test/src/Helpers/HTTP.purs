module Helpers.HTTP where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff, attempt)
import Helpers.CreateString as C
import Helpers.CreateString (makeUrlAndUnwrap)
import Helpers.Types (Node, ResWithBody(..))
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Prim.Row (class Union)
import Shared.Rtsv2.Chaos as Chaos
import Shared.Rtsv2.Router.Endpoint (Endpoint(..), makeUrlAddr, makeUrl)
import Shared.Rtsv2.Stream (RtmpShortName, SlotId, SlotNameAndProfileName(..), SlotRole(..), RtmpStreamName, ProfileName)
import Shared.Rtsv2.Types (CanaryState(..), RunState(..), CurrentLoad(..), ServerAddress(..))
import Simple.JSON as SimpleJSON
import Toppokki as T


playerUrl :: Node -> SlotId -> SlotRole -> T.URL
playerUrl node slotId slotRole =
  T.URL $ unwrap $ makeUrl (C.mkServerAddress node) $ ClientPlayerE slotId slotRole

ingestUrl :: Node -> RtmpShortName -> RtmpStreamName -> T.URL
ingestUrl node shortName streamName =
  T.URL $ unwrap $ makeUrl (C.mkServerAddress node) $ ClientWebRTCIngestE shortName streamName

canaryPlayerUrl :: Node -> SlotId -> SlotRole -> T.URL
canaryPlayerUrl node slotId slotRole =
  T.URL $ unwrap $ makeUrl (C.mkServerAddress node) $ CanaryClientPlayerE slotId slotRole

canaryIngestUrl :: Node -> RtmpShortName -> RtmpStreamName -> T.URL
canaryIngestUrl node shortName streamName =
  T.URL $ unwrap $ makeUrl (C.mkServerAddress node) $ CanaryClientWebRTCIngestE shortName streamName

fetch
  :: forall options trash.
  Union options trash M.Options
  => M.URL
  -> Record (method :: M.Method | options)
  -> Aff (Either String ResWithBody)
fetch url options =
  M.fetch nodeFetch url options # attempt >>= standarise
  where
   standarise (Right response) = do
     body <- M.text response
     pure $ Right { headers   : M.headers response
                  , statusCode : M.statusCode response
                  , body
                  }
   standarise (Left axError) = pure $ Left $ show axError

get :: M.URL -> Aff (Either String ResWithBody)
get url = fetch url { method: M.getMethod }

-- | Processes
healthCheck :: Node -> Aff (Either String ResWithBody)
healthCheck node =
  get (M.URL $ makeUrlAndUnwrap node HealthCheckE)

changeCanaryState :: Node -> CanaryState -> Aff (Either String ResWithBody)
changeCanaryState node canary =
  fetch (M.URL $ makeUrlAndUnwrap node CanaryE)
        { method: M.postMethod
        , body: SimpleJSON.writeJSON canary
        , headers: M.makeHeaders { "Content-Type": "application/json" }
        }

changeRunState :: Node -> RunState -> Aff (Either String ResWithBody)
changeRunState node runState =
  fetch (M.URL $ makeUrlAndUnwrap node RunStateE)
        { method: M.postMethod
        , body: SimpleJSON.writeJSON runState
        , headers: M.makeHeaders { "Content-Type": "application/json" }
        }

clientStart :: Node -> CanaryState -> SlotId -> Aff (Either String ResWithBody)
clientStart node canary slotId =
  fetch (M.URL $ makeUrlAndUnwrap node (ClientStartE canary slotId Primary))
        { method: M.postMethod
        , body: "{}"
        , headers: M.makeHeaders { "Content-Type": "application/json" }
        }

clientStop :: String -> Node -> SlotId -> Aff (Either String ResWithBody)
clientStop clientId node slotId  =
  fetch (M.URL $ makeUrlAndUnwrap node (ClientStopE slotId Primary clientId))
        { method: M.postMethod
        , body: "{}"
        , headers: M.makeHeaders { "Content-Type": "application/json" }
        }

killProcessNode :: Node -> Chaos.ChaosPayload -> Aff (Either String ResWithBody)
killProcessNode node chaos =
  fetch (M.URL $ makeUrlAndUnwrap node (Chaos))
        { method: M.postMethod
        , body: SimpleJSON.writeJSON chaos
        , headers: M.makeHeaders { "Content-Type": "application/json" }
        }

killProcessServerAddr :: ServerAddress -> Chaos.ChaosPayload -> Aff (Either String ResWithBody)
killProcessServerAddr addr chaos =
  fetch (M.URL $ unwrap $ makeUrlAddr addr (Chaos))
        { method: M.postMethod
        , body: SimpleJSON.writeJSON (chaos :: Chaos.ChaosPayload)
        , headers: M.makeHeaders { "Content-Type": "application/json" }
        }

getLoad :: Node -> Aff (Either String ResWithBody)
getLoad node = get  (M.URL $ makeUrlAndUnwrap node LoadE)

setLoad :: Node -> Number -> Aff (Either String ResWithBody)
setLoad node load =
  fetch (M.URL $ makeUrlAndUnwrap node LoadE)
        { method: M.postMethod
        , body: SimpleJSON.writeJSON $ CurrentLoad {cpu: wrap load, network: wrap 0}
        , headers: M.makeHeaders { "Content-Type": "application/json" }
        }

dropAgentMessages :: Node -> Boolean -> Aff (Either String ResWithBody)
dropAgentMessages node flag =
  fetch (M.URL $ makeUrlAndUnwrap node IntraPoPTestHelperE)
        { method: M.postMethod
        , body: "{\"dropAgentMessages\": " <> show flag <> "}"
        , headers: M.makeHeaders { "Content-Type": "application/json" }
        }

-- | Stats
getStats :: Node -> Endpoint -> Aff (Either String ResWithBody)
getStats node route = get (M.URL $ makeUrlAndUnwrap node route)


-- | Ingest
getEgestStats :: Node -> SlotId -> Aff (Either String ResWithBody)
getEgestStats node slotId = getStats node (EgestStatsE slotId Primary)

ingestStart :: Node -> CanaryState -> RtmpShortName -> RtmpStreamName -> Aff (Either String ResWithBody)
ingestStart node canary shortName streamName =
  get (M.URL $ makeUrlAndUnwrap node (IngestStartE canary shortName streamName))

ingestStop :: Node -> SlotId -> ProfileName -> Aff (Either String ResWithBody)
ingestStop node slotId profileName =
  get (M.URL $ makeUrlAndUnwrap node (IngestStopE slotId Primary profileName))

-- | Aggregators
getAggregatorStats :: Node -> SlotId -> Aff (Either String ResWithBody)
getAggregatorStats node slotId = getStats node (IngestAggregatorE slotId Primary)


-- | Relays
getRelayStats :: Node -> SlotId -> Aff (Either String ResWithBody)
getRelayStats node slotId = getStats node (RelayStatsE slotId Primary)

getProxiedRelayStats :: Node -> SlotId -> Aff (Either String ResWithBody)
getProxiedRelayStats node slotId = getStats node (RelayProxiedStatsE slotId Primary)


-- | PoP
getIntraPoPState :: Node -> Aff (Either String ResWithBody)
getIntraPoPState node = get (M.URL $ makeUrlAndUnwrap node ServerStateE)


-- | Slot
getSlotState :: Node -> SlotId -> Aff (Either String ResWithBody)
getSlotState node slotId = get (M.URL $ makeUrlAndUnwrap node (SlotStateE slotId))
