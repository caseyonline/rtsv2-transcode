module Rtsv2.Agents.StreamRelay.Instance
  ( startLink
  , isAvailable
  , registerEgest
  , registerRelayChain
  , init
  , status
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, toUnfoldable)
import Data.Set as Set
import Data.Traversable (traverse_)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, uncons, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Logger (Logger, spy)
import Logger as Logger
import Pinto (ServerName, StartLinkResult, isRegistered)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import PintoHelper (exposeState)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.StreamRelay.DownstreamProxy as DownstreamProxy
import Rtsv2.Agents.StreamRelay.Types (CreateRelayPayload, RegisterEgestPayload, SourceRoute, RegisterRelayChainPayload)
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (EgestServer, PoPName, RelayServer, Server, extractAddress)
import Shared.Types.Agent.State as PublicState


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
init payload@{streamId} = do
  logInfo "StreamRelay starting" {payload}
  thisServer <- PoPDefinition.getThisServer
  IntraPoP.announceRelayIsAvailable streamId
  -- TODO - linger timeout / exit if idle
  pure { streamId: streamId
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
      logInfo "Register egest " {payload}
      newState <- maybeStartRelaysForEgest state{ egestsServed = Set.insert payload.deliverTo egestsServed}
      pure $ CallReply unit $ spy "registerEgest" newState

maybeStartRelaysForEgest :: State -> Effect State
maybeStartRelaysForEgest state@{egestSourceRoutes: Just _} = pure state
maybeStartRelaysForEgest state@{streamId, aggregatorPoP, thisServer} = do
  relayRoutes <- TransPoP.routesTo aggregatorPoP
  _ <- traverse_ (registerWithRelayProxy streamId aggregatorPoP) relayRoutes
  pure state{egestSourceRoutes = Just (spy "relayRoutes" relayRoutes)}


registerRelayChain :: RegisterRelayChainPayload -> Effect Unit
registerRelayChain payload = Gen.doCast (serverName payload.streamId)
  \state@{streamId, relaysServed, egestSourceRoutes, aggregatorPoP} -> do
    logInfo "Register relay chain " {payload}
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
      _ <- registerWithRelayProxy streamId aggregatorPoP payload.sourceRoute
      pure $ Gen.CastNoReply state {relaysServed = newRelaysServed}



registerWithRelayProxy :: StreamId -> PoPName -> SourceRoute -> Effect Unit
registerWithRelayProxy streamId aggregatorPoP sourceRoute = do
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
