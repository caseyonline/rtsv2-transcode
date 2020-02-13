module Rtsv2.Agents.StreamRelay.Instance
  ( startLink
  , isAvailable
  , registerEgest
  , registerRelay
  , init
  , status
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
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
import Rtsv2.Agents.StreamRelay.Types (CreateRelayPayload, RegisterEgestPayload, SourceRoute, RegisterRelayPayload)
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.Stream (RelayKey(..), StreamId, StreamRole)
import Shared.Types (EgestServer, PoPName, RelayServer, Server, extractAddress)
import Shared.Types.Agent.State as PublicState


type State
  = { relayKey :: RelayKey
    , aggregatorPoP :: PoPName
    , thisServer :: Server
    , relaysServed :: Map RelayServer (Set SourceRoute)
    , egestsServed :: Set EgestServer
    , egestSourceRoutes :: Maybe (List SourceRoute)
    }


payloadToRelayKey :: forall r.
  { streamId :: StreamId
  , streamRole :: StreamRole
  | r
  }
  -> RelayKey
payloadToRelayKey payload = RelayKey payload.streamId payload.streamRole

serverName :: RelayKey -> ServerName State Unit
serverName = Names.streamRelayInstanceName

startLink :: CreateRelayPayload -> Effect StartLinkResult
startLink payload =
  let
    relayKey = payloadToRelayKey payload
  in
  Gen.startLink (serverName relayKey) (init payload) Gen.defaultHandleInfo

isAvailable :: RelayKey -> Effect Boolean
isAvailable relayKey = isRegistered (serverName relayKey)

status  :: RelayKey -> Effect (PublicState.StreamRelay List)
status =
  exposeState mkStatus <<< serverName
  where
    mkStatus state =
       { egestsServed : extractAddress <$> Set.toUnfoldable state.egestsServed
       , relaysServed : extractAddress <$> Map.keys state.relaysServed
       }

init :: CreateRelayPayload -> Effect State
init payload@{streamId, streamRole} = do
  let
    relayKey = RelayKey streamId streamRole
  logInfo "StreamRelay starting" {payload}
  thisServer <- PoPDefinition.getThisServer

  IntraPoP.announceLocalRelayIsAvailable relayKey
  -- TODO - linger timeout / exit if idle
  pure { relayKey: relayKey
       , aggregatorPoP : payload.aggregatorPoP
       , thisServer
       , relaysServed : Map.empty
       , egestsServed : mempty
       , egestSourceRoutes : Nothing
       }

registerEgest :: RegisterEgestPayload -> Effect Unit
registerEgest payload = Gen.doCall (serverName $ payloadToRelayKey payload) doRegisterEgest
  where
    doRegisterEgest :: State -> Effect (CallResult Unit State)
    doRegisterEgest state@{egestsServed} = do
      logInfo "Register egest " {payload}
      newState <- maybeStartRelaysForEgest state{ egestsServed = Set.insert payload.deliverTo egestsServed}
      pure $ CallReply unit $ spy "registerEgest" newState

maybeStartRelaysForEgest :: State -> Effect State
maybeStartRelaysForEgest state@{egestSourceRoutes: Just _} = pure state
maybeStartRelaysForEgest state@{relayKey, aggregatorPoP, thisServer} = do
  relayRoutes <- TransPoP.routesTo aggregatorPoP
  traverse_ (registerWithRelayProxy relayKey aggregatorPoP) relayRoutes
  pure state{egestSourceRoutes = Just (spy "relayRoutes" relayRoutes)}


registerRelay :: RegisterRelayPayload -> Effect Unit
registerRelay payload = Gen.doCast (serverName $ payloadToRelayKey payload)
  \state@{relayKey, relaysServed, egestSourceRoutes, aggregatorPoP} -> do
    logInfo "Register relay chain " {payload}
    -- Do we already have this registration
    let
      routesViaThisServer = fromMaybe Set.empty $ Map.lookup payload.deliverTo relaysServed
    if Set.member payload.sourceRoute routesViaThisServer
    then do
      logWarning "Duplicate Relay Registration" {payload}
      pure $ Gen.CastNoReply state
    else do
      let
        newRelaysServed = Map.insert payload.deliverTo (Set.insert payload.sourceRoute routesViaThisServer) relaysServed
      registerWithRelayProxy relayKey aggregatorPoP payload.sourceRoute
      pure $ Gen.CastNoReply state {relaysServed = newRelaysServed}



registerWithRelayProxy :: RelayKey -> PoPName -> SourceRoute -> Effect Unit
registerWithRelayProxy relayKey@(RelayKey streamId streamRole) aggregatorPoP sourceRoute = do
  case uncons sourceRoute of
    Just {head: nextPoP, tail: remainingRoute} -> do
      -- TODO okAlreadyStarted
      _ <- DownstreamProxy.startLink {streamId, streamRole, proxyFor: nextPoP, aggregatorPoP}
      -- TODO ok
      DownstreamProxy.addRelayRoute relayKey  nextPoP remainingRoute
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
