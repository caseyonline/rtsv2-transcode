module Rtsv2.Agents.StreamRelayInstance
  ( startLink
  , isAvailable
  , registerEgest
  , init
  , status
  , CreateRelayPayload
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set (Set, toUnfoldable)
import Data.Set as Set
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Logger (Logger, spy)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import PintoHelper (exposeState)
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Utils (crashIfLeft)
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (EgestServer, PoPName(..), RelayServer, Server, extractAddress, extractPoP)
import Shared.Types.Agent.State as PublicState
import SpudGun as SpudGun

type CreateRelayPayload
  = { streamId :: StreamId
    , aggregator :: Server
    }

-- basically a listzipper...
type RelayRoute
  = { toEgest :: List PoPName
    , thisPoP :: PoPName
    , toSource :: List PoPName
    }

type State
  = { streamId :: StreamId
    , aggregator :: Server
    , thisServer :: Server
    -- , primaryRelayRoutes :: Maybe (List ViaPoPs)
    -- , primaryNeighbours :: Maybe (List Server)
    -- , secondaryRelayCount :: Int
    -- , secondaryRelayRoutes :: Maybe (List ViaPoPs)
    , relaysServed :: Set RelayServer
    , egestsServed :: Set EgestServer
    , egestSourceRoutes :: Maybe (List RelayRoute)
    }

serverName :: StreamId -> ServerName State Unit
serverName streamId = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "streamRelay" streamId))

startLink :: CreateRelayPayload -> Effect StartLinkResult
startLink payload = Gen.startLink (serverName payload.streamId) (init payload) Gen.defaultHandleInfo

isAvailable :: StreamId -> Effect Boolean
isAvailable streamId = isRegistered (serverName streamId)

status  :: StreamId -> Effect PublicState.StreamRelay
status =
  exposeState mkStatus <<< serverName
  where
    mkStatus :: State -> PublicState.StreamRelay
    mkStatus state =
       {egestsServed : extractAddress <$> toUnfoldable state.egestsServed}


init :: CreateRelayPayload -> Effect State
init payload = do
  _ <- logInfo "StreamRelay starting" {payload}
  thisServer <- PoPDefinition.getThisServer
  pure { streamId: payload.streamId
       , aggregator : payload.aggregator
       , thisServer
       , relaysServed : mempty
       , egestsServed : mempty
       , egestSourceRoutes : Nothing
       }

registerEgest :: StreamId -> EgestServer -> Effect Unit
registerEgest streamId egestServer = Gen.doCall (serverName streamId) $ doRegisterEgest egestServer

doRegisterEgest :: EgestServer -> State -> Effect (CallResult Unit State)
doRegisterEgest egestServer state@{egestsServed} = do
  _ <- logInfo "Register egest " {egestServer}
  newState <- maybeStartEgestRelays state{ egestsServed = Set.insert egestServer egestsServed}
  pure $ CallReply unit newState


maybeStartEgestRelays :: State -> Effect State
maybeStartEgestRelays state@{egestSourceRoutes: Just _} = pure state
maybeStartEgestRelays state@{streamId, aggregator, thisServer} = do
  relayRoutes <- (map toRelayRoute) <$> TransPoP.routesTo (spy "aggPoP" $ extractPoP aggregator)
--  _ <- traverse startRelay


  let _ = spy "thisServer" thisServer
  pure state{egestSourceRoutes = Just (spy "relayRoutes" relayRoutes)}
  where
    toRelayRoute :: List PoPName -> RelayRoute
    toRelayRoute pops = { toSource : pops
                        , thisPoP  : extractPoP thisServer
                        , toEgest  : nil
                        }



-- startRelayChain :: StreamId -> RelayRoute -> Effect Unit
-- startRelay streamId {toSource, thisPoP, toEgest} =
--   case uncons toSource of

--     Nothing -> pure Unit
--     Just {head, tail} -> do
--       -- TODO - strategy for knowing which servers in a remote PoP are healthy
--       -- Maybe always route via the transPoP leader
--       popLeader <- TransPoP.getLeaderFor head
--       let
--         url = makeUrl idleServer RelayChainE
--         payload = {streamId, aggregator} :: CreateRelayPayload

--         url = makeUrl popLeader  EgestE
--         payload = {streamId, toSouce: tail, toEgest: (thisPoP : toEgest), thisPoP: head}

--   do

--   void <$> crashIfLeft =<< SpudGun.postJson url ({streamId, fullRoute, remainingRoute} :: CreateEgestPayload)



--------------------------------------------------------------------------------
-- Minimal zipper helpers
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.StreamRelay :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
