module Helpers.HTTP where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff, attempt)
import Helpers.CreateString (makeUrlAndUnwrap)
import Helpers.Types (Node, ResWithBody(..))
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Prim.Row (class Union)
import Shared.Chaos as Chaos
import Shared.Router.Endpoint (Canary(..), Endpoint(..), makeUrlAddr)
import Shared.Stream (RtmpShortName, SlotId, SlotNameAndProfileName(..), SlotRole(..))
import Shared.Types (ServerAddress(..))
import Simple.JSON as SimpleJSON


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
clientStart :: Node -> SlotId -> Aff (Either String ResWithBody)
clientStart node slotId =
  fetch (M.URL $ makeUrlAndUnwrap node (ClientStartE Live slotId Primary))
        { method: M.postMethod
        , body: "{}"
        , headers: M.makeHeaders { "Content-Type": "application/json" }
        }

clientStop :: String -> Node -> SlotId -> Aff (Either String ResWithBody)
clientStop clientId node slotId  =
  fetch (M.URL $ makeUrlAndUnwrap node (ClientStopE Live slotId Primary clientId))
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

setLoad :: Node -> Number -> Aff (Either String ResWithBody)
setLoad node load =
  fetch (M.URL $ makeUrlAndUnwrap node LoadE)
        { method: M.postMethod
        , body: "{\"load\": " <> show load <> "}"
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

ingestStart :: Node -> RtmpShortName -> SlotNameAndProfileName -> Aff (Either String ResWithBody)
ingestStart node shortName profileName =
  get (M.URL $ makeUrlAndUnwrap node (IngestStartE Live shortName profileName))

ingestStop :: Node -> SlotId -> SlotNameAndProfileName -> Aff (Either String ResWithBody)
ingestStop node slotId (SlotNameAndProfileName _ profileName) =
  get (M.URL $ makeUrlAndUnwrap node (IngestStopE Live slotId Primary profileName))


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
