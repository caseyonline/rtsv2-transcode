module Rtsv2.Agents.StreamRelayDownstreamProxy
  ( startLink
  , addRelayRoute
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.List as List
import Erl.Data.Tuple (fst, snd)
import Erl.Process (spawnLink)
import Erl.Utils as Erl
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen as Gen
import Rtsv2.Agents.StreamRelayTypes (CreateProxyPayload, CreateRelayPayload, SourceRoute)
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrlAddr)
import Shared.Agent as Agent
import Shared.Stream (RelayKey(..), SlotId, SlotRole)
import Shared.Types (PoPName, Server, ServerAddress)
import SpudGun as SpudGun


data Msg
  = Unit


type ProxyRoute = List PoPName

type State
  = { relayKey :: RelayKey
    , proxyFor :: PoPName
    , thisServer :: Server
    , proxiedServer :: Maybe ServerAddress
    , aggregator :: Server
    , routesThroughThisProxy :: Set ProxyRoute
    }


serverName :: RelayKey -> PoPName -> ServerName State Msg
serverName = Names.streamRelayDownstreamProxyName


payloadToRelayKey :: forall r.
  { slotId :: SlotId
  , streamRole :: SlotRole
  | r
  }
  -> RelayKey
payloadToRelayKey payload = RelayKey payload.slotId payload.streamRole

startLink :: CreateProxyPayload -> Effect StartLinkResult
startLink payload = Gen.startLink (serverName (payloadToRelayKey payload) payload.proxyFor) (init payload) Gen.defaultHandleInfo

init :: CreateProxyPayload -> Effect State
init payload@{proxyFor, aggregator} = do
  let relayKey = payloadToRelayKey payload
  logInfo "streamRelayDownstreamProxy starting" {payload}
  void $ spawnLink (\_ -> connect relayKey proxyFor aggregator)
  -- TODO - monitor our parent
  thisServer <- PoPDefinition.getThisServer
  pure { relayKey
       , proxyFor
       , thisServer
       , aggregator
       , routesThroughThisProxy: mempty
       , proxiedServer: Nothing
       }


setProxyServer :: RelayKey -> PoPName -> ServerAddress -> Effect Unit
setProxyServer relayKey popName serverAddr = Gen.doCast (serverName relayKey popName)
  \state@{routesThroughThisProxy} -> Gen.CastNoReply <$> do
    let newState = state{proxiedServer = Just serverAddr}
    traverse_ (maybeCallProxyWithRoute newState) routesThroughThisProxy
    pure newState


maybeCallProxyWithRoute :: State -> SourceRoute -> Effect Unit
maybeCallProxyWithRoute {proxiedServer: Nothing} _ = pure unit
maybeCallProxyWithRoute {relayKey: RelayKey slotId streamRole, thisServer, proxiedServer: Just remoteRelay} sourceRoute = do
  let
    payload = {slotId, streamRole, deliverTo: thisServer, sourceRoute}
    url = makeUrlAddr remoteRelay RelayRegisterRelayE
  void $ spawnLink (\_ -> do
                     -- TODO - send message to parent saying the register worked?
                     -- Retry loop?  DOn't use follow here are we should be talking to the
                     -- correct server directly
                     logInfo "Registering route" {payload}
                     void $ SpudGun.postJson url payload
                   )

addRelayRoute :: RelayKey -> PoPName -> List PoPName -> Effect Unit
addRelayRoute relayKey popName route = Gen.doCast (serverName relayKey popName)
  \state@{routesThroughThisProxy, proxiedServer} -> Gen.CastNoReply <$> do
      if Set.member route routesThroughThisProxy
      then do
        _ <- logWarning "Duplicate Registration" {relayKey, popName, route}
        pure state
      else do
        let
          newState = state{routesThroughThisProxy = Set.insert route routesThroughThisProxy}
        maybeCallProxyWithRoute newState route
        pure newState


connect :: RelayKey -> PoPName -> Server -> Effect Unit
connect relayKey@(RelayKey slotId streamRole) proxyFor aggregator = do
        mRandomAddr <- PoPDefinition.getRandomServerInPoP proxyFor
        case mRandomAddr of
          Nothing -> do
            _ <- logWarning "No random server found in" {proxyFor}
            retrySleep
            connect relayKey proxyFor aggregator

          Just randomAddr -> do
            let
              payload = {slotId, streamRole, aggregator} :: CreateRelayPayload
              url = makeUrlAddr randomAddr RelayEnsureStartedE
            resp <- SpudGun.postJsonFollow url payload
            case resp of
              Right (SpudGun.SpudResponse sc headers body) -> do
                let
                  mServedBy = List.head $ filterMap (\hdrTuple -> if fst hdrTuple == "x-servedby" then Just (snd hdrTuple) else Nothing) headers
                case mServedBy of
                  Nothing -> do
                    _ <- logError "x-servedby header missing in StreamRelayDownstreamProxy" {resp}
                    -- TODO - crash??
                    retrySleep
                    connect relayKey proxyFor aggregator
                  Just addr -> do
                    logInfo "Located relay to proxy" {relayKey, proxyFor, aggregator, addr}
                    setProxyServer relayKey proxyFor $ wrap addr
              Left _ -> do
                _ <- logWarning "Error returned from ensureStarted request" {resp, relayKey, proxyFor, aggregator}
                retrySleep
                connect relayKey proxyFor aggregator
  where
    retrySleep :: Effect Unit
    retrySleep = Erl.sleep (wrap 500)

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.StreamRelay :  "Instance" : "Proxy" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

logError :: forall a. Logger a
logError= domainLog Logger.error

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
