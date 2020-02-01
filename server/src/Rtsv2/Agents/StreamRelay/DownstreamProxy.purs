module Rtsv2.Agents.StreamRelay.DownstreamProxy
  ( startLink
  , addRelayRoute
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.List as List
import Erl.Data.Tuple (fst, snd)
import Erl.Utils as Erl
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.StreamRelay.Types (CreateRelayPayload, CreateProxyPayload)
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrlAddr)
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (PoPName, Server, ServerAddress)
import SpudGun as SpudGun


data Msg
  = Connect


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
startLink payload = Gen.startLink (serverName payload.streamId payload.proxyFor) (init payload) handleInfo

init :: CreateProxyPayload -> Effect State
init payload@{streamId, proxyFor, aggregatorPoP} = do
  _ <- logInfo "streamRelayDownstreamProxy starting" {payload}
  _ <- Timer.sendAfter (serverName streamId proxyFor) 0 Connect
  -- TODO - monitor our parent
  thisServer <- PoPDefinition.getThisServer
  pure { streamId
       , proxyFor
       , thisServer
       , aggregatorPoP
       , routesThroughThisProxy: mempty
       , proxiedServer: Nothing
       }


addRelayRoute :: StreamId -> PoPName -> List PoPName -> Effect Unit
addRelayRoute streamId popName route = Gen.doCast (serverName streamId popName)
  \state@{routesThroughThisProxy} -> Gen.CastNoReply <$> do
      if Set.member route routesThroughThisProxy
      then do
        _ <- logWarning "Duplicate Registration" {streamId, popName, route}
        pure state
      else
        pure state{routesThroughThisProxy = Set.insert route routesThroughThisProxy}



handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{streamId, proxyFor, aggregatorPoP} = case msg of
  Connect -> CastNoReply <$> connectRetry
    where
      retrySleep :: Effect Unit
      retrySleep = Erl.sleep (wrap 500)

      connectRetry :: Effect State
      connectRetry = do
        -- TODO - strategy for knowing which servers in a remote PoP are healthy
        -- for now route via the transPoP leader
        mPoPLeader <- TransPoP.getLeaderFor proxyFor
        case mPoPLeader of
          Nothing -> do
            _ <- logWarning "No PoP Leader known for" {proxyFor}
            retrySleep
            connectRetry

          Just popLeader -> do
            let
              payload = {streamId, aggregatorPoP} :: CreateRelayPayload
              url = makeUrlAddr popLeader RelayEnsureStartedE
            resp <- SpudGun.postJson url payload
            case resp of
              Right (SpudGun.SpudResponse sc headers body) -> do
                let
                  mServedBy = List.head $ filterMap (\hdrTuple -> if fst hdrTuple == "x-servedby" then Just (snd hdrTuple) else Nothing) headers
                case mServedBy of
                  Nothing -> do
                    _ <- logError "x-servedby header missing in StreamRelayDownstreamProxy" {resp}
                    -- TODO - crash??
                    retrySleep
                    connectRetry
                  Just addr ->
                    pure $ state{proxiedServer = Just $ wrap addr}
              Left _ -> do
                _ <- logWarning "Error returned from ensureStarted request" {resp, state}
                retrySleep
                connectRetry


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
