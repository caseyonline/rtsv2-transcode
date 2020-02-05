module Rtsv2.Agents.StreamRelay.DownstreamProxy
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
import Rtsv2.Agents.StreamRelay.Types (CreateProxyPayload, CreateRelayPayload, SourceRoute)
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrlAddr)
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (PoPName, Server, ServerAddress)
import SpudGun as SpudGun


data Msg
  = Unit


type ProxyRoute = List PoPName

type State
  = { streamId :: StreamId
    , proxyFor :: PoPName
    , thisServer :: Server
    , proxiedServer :: Maybe ServerAddress
    , aggregatorPoP :: PoPName
    , routesThroughThisProxy :: Set ProxyRoute

    }


serverName :: StreamId -> PoPName -> ServerName State Msg
serverName = Names.streamRelayDownstreamProxyName

startLink :: CreateProxyPayload -> Effect StartLinkResult
startLink payload = Gen.startLink (serverName payload.streamId payload.proxyFor) (init payload) Gen.defaultHandleInfo

init :: CreateProxyPayload -> Effect State
init payload@{streamId, proxyFor, aggregatorPoP} = do
  logInfo "streamRelayDownstreamProxy starting" {payload}
  void $ spawnLink (\_ -> connect streamId proxyFor aggregatorPoP)
  -- TODO - monitor our parent
  thisServer <- PoPDefinition.getThisServer
  pure { streamId
       , proxyFor
       , thisServer
       , aggregatorPoP
       , routesThroughThisProxy: mempty
       , proxiedServer: Nothing
       }


setProxyServer :: StreamId -> PoPName -> ServerAddress -> Effect Unit
setProxyServer streamId popName serverAddr = Gen.doCast (serverName streamId popName)
  \state@{routesThroughThisProxy} -> Gen.CastNoReply <$> do
    let newState = state{proxiedServer = Just serverAddr}
    traverse_ (maybeCallProxyWithRoute newState) routesThroughThisProxy
    pure newState


maybeCallProxyWithRoute :: State -> SourceRoute -> Effect Unit
maybeCallProxyWithRoute {proxiedServer: Nothing} _ = pure unit
maybeCallProxyWithRoute {streamId, thisServer, proxiedServer: Just remoteRelay} sourceRoute = do
  let
    payload = {streamId, deliverTo: thisServer, sourceRoute}
    url = makeUrlAddr remoteRelay RelayChainE
  void $ spawnLink (\_ -> do
                     -- TODO - send message to parent saying the register worked?
                     -- Retry loop?  DOn't use follow here are we should be talking to the
                     -- correct server directly
                     logInfo "Registering route" {payload}
                     void $ SpudGun.postJson url payload
                   )

addRelayRoute :: StreamId -> PoPName -> List PoPName -> Effect Unit
addRelayRoute streamId popName route = Gen.doCast (serverName streamId popName)
  \state@{routesThroughThisProxy, proxiedServer} -> Gen.CastNoReply <$> do
      if Set.member route routesThroughThisProxy
      then do
        _ <- logWarning "Duplicate Registration" {streamId, popName, route}
        pure state
      else do
        let
          newState = state{routesThroughThisProxy = Set.insert route routesThroughThisProxy}
        maybeCallProxyWithRoute newState route
        pure newState


connect :: StreamId -> PoPName -> PoPName -> Effect Unit
connect streamId proxyFor aggregatorPoP = do
        mRandomAddr <- PoPDefinition.getRandomServerInPoP proxyFor
        case mRandomAddr of
          Nothing -> do
            _ <- logWarning "No random server found in" {proxyFor}
            retrySleep
            connect streamId proxyFor aggregatorPoP

          Just randomAddr -> do
            let
              payload = {streamId, aggregatorPoP} :: CreateRelayPayload
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
                    connect streamId proxyFor aggregatorPoP
                  Just addr -> do
                    logInfo "Located relay to proxy" {streamId, proxyFor, aggregatorPoP, addr}
                    setProxyServer streamId proxyFor $ wrap addr
              Left _ -> do
                _ <- logWarning "Error returned from ensureStarted request" {resp, streamId, proxyFor, aggregatorPoP}
                retrySleep
                connect streamId proxyFor aggregatorPoP
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
