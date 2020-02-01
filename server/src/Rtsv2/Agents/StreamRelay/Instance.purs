module Rtsv2.Agents.StreamRelay.Instance
  ( startLink
  , isAvailable
  , registerEgest
  , registerRelayChain
  , init
  , status
  ) where

import Prelude

import Data.Either (isRight)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Set (Set, toUnfoldable)
import Data.Set as Set
import Data.Traversable (traverse_)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, catMaybes, head, nil, uncons, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process (spawnLink)
import Erl.Utils as Erl
import Foreign (unsafeToForeign)
import Logger (Logger, spy)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import PintoHelper (exposeState)
import Rtsv2.Agents.StreamRelay.DownstreamProxy as DownstreamProxy
import Rtsv2.Agents.StreamRelay.Types (CreateRelayPayload, RegisterEgestPayload, SourceRoute, RegisterRelayChainPayload)
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrlAddr)
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (EgestServer, PoPName, RelayServer, Server, ServerAddress(..), extractAddress, extractPoP)
import Shared.Types.Agent.State as PublicState
import SpudGun as SpudGun


type State
  = { streamId :: StreamId
    , aggregatorPoP :: PoPName
    , thisServer :: Server
    , relaysServed :: Map RelayServer (Set SourceRoute)
    , egestsServed :: Set EgestServer
    , egestSourceRoutes :: Maybe (List SourceRoute)
    }

serverName :: StreamId -> ServerName State Unit
serverName = Names.streamRelayInstanceName

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
       { egestsServed : extractAddress <$> toUnfoldable state.egestsServed
       , relaysServed : [] --extractAddress <$> toUnfoldable state.relaysServed
       }

init :: CreateRelayPayload -> Effect State
init payload = do
  _ <- logInfo "StreamRelay starting" {payload}
  thisServer <- PoPDefinition.getThisServer
  pure { streamId: payload.streamId
       , aggregatorPoP : payload.aggregatorPoP
       , thisServer
       , relaysServed : Map.empty
       , egestsServed : mempty
       , egestSourceRoutes : Nothing
       }

registerEgest :: RegisterEgestPayload -> Effect Unit
registerEgest payload = Gen.doCall (serverName payload.streamId) doRegisterEgest
  where
    doRegisterEgest :: State -> Effect (CallResult Unit State)
    doRegisterEgest state@{egestsServed} = do
      _ <- logInfo "Register egest " {payload}
      newState <- maybeStartRelaysForEgest state{ egestsServed = Set.insert payload.deliverTo egestsServed}
      pure $ CallReply unit $ spy "registerEgest" newState

maybeStartRelaysForEgest :: State -> Effect State
maybeStartRelaysForEgest state@{egestSourceRoutes: Just _} = pure state
maybeStartRelaysForEgest state@{streamId, aggregatorPoP, thisServer} = do
  relayRoutes <- TransPoP.routesTo aggregatorPoP
  _ <- traverse_ (maybeStartRelayProxies streamId aggregatorPoP) relayRoutes
  pure state{egestSourceRoutes = Just (spy "relayRoutes" relayRoutes)}


-- egestSourceNeighbours :: Maybe (List RelayRoute) -> Set (Maybe PoPName)
-- egestSourceNeighbours Nothing = Set.empty
-- egestSourceNeighbours (Just egestSourceRoutes) =
--   Set.fromFoldable (head <<< _.toSource <$> egestSourceRoutes)

registerRelayChain :: RegisterRelayChainPayload -> Effect Unit
registerRelayChain payload = Gen.doCast (serverName payload.streamId)
  \state@{streamId, relaysServed, egestSourceRoutes, aggregatorPoP} -> do
    _ <- logInfo "Register relay chain " {payload}
    -- Do we already have this registration
    let
      routesViaThisServer = fromMaybe Set.empty $ Map.lookup payload.deliverTo relaysServed
    if Set.member payload.sourceRoute routesViaThisServer
    then do
      _ <- logWarning "Duplicate Relay Registration" {payload}
      pure $ Gen.CastNoReply state
    else do
      let
        newRelaysServed = Map.insert payload.deliverTo (Set.insert payload.sourceRoute routesViaThisServer) relaysServed
      _ <- maybeStartRelayProxies streamId aggregatorPoP payload.sourceRoute
      pure $ Gen.CastNoReply state {relaysServed = newRelaysServed}



maybeStartRelayProxies streamId aggregatorPoP sourceRoute = do
  case uncons sourceRoute of
    Just {head: nextPoP, tail: remainingRoute} -> do
      -- TODO okAlreadyStarted
      _ <- DownstreamProxy.startLink {streamId, proxyFor: nextPoP, aggregatorPoP}
      -- TODO ok
      DownstreamProxy.addRelayRoute streamId  nextPoP remainingRoute
    Nothing ->
      -- This relay request is for content aggregated in this popdefinition
      -- TODO - media stuff
      pure unit





-- walkRelayChain :: StreamId -> PoPName -> SourceRoute -> Effect Unit
-- walkRelayChain streamId aggregatorPoP {toSource, thisPoP, toEgest} = do
--   case uncons toSource of
--     Nothing -> pure unit
--     Just {head, tail} -> do
--       let
--         thePayload :: RegisterRelayChainPayload
--         thePayload = { streamId
--                      , aggregatorPoP
--                      , route : { toEgest: (thisPoP : toEgest)
--                                , thisPoP: head
--                                , toSource: tail
--                                }
--                      }
--       void $ spawnLink (\_ -> notifyNextInChain thePayload)
--   where
--     notifyNextInChain :: RegisterRelayChainPayload -> Effect Unit
--     notifyNextInChain payload@{route: {thisPoP: nextInChain}} = do
--       -- TODO - strategy for knowing which servers in a remote PoP are healthy
--       -- for now route via the transPoP leader
--       mPoPLeader <- TransPoP.getLeaderFor nextInChain
--       case mPoPLeader of
--         Nothing -> do
--           _ <- logWarning "No PoP Leader for relay chain" {nextInChain}
--           notifyNextInChain payload
--         Just popLeader -> do
--           let
--             url = makeUrlAddr popLeader RelayChainE
--           resp <- SpudGun.postJson url payload
--           if isRight resp then
--             pure unit
--           else do
--             _ <- logWarning "Bad return code from post to create relay chain" {nextInChain, resp}
--             -- TODO sleep duration from config
--             _ <- Erl.sleep (wrap 500)
--             notifyNextInChain payload





--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.StreamRelay :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
