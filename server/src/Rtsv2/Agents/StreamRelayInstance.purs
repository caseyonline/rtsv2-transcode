module Rtsv2.Agents.StreamRelayInstance
  ( startLink
  , isAvailable
  , registerEgest
  , registerRelayChain
  , init
  , status
  , CreateRelayPayload
  , RegisterRelayChainPayload
  , RelayRoute
  , RegisterEgestPayload
  ) where

import Prelude

import Data.Either (isRight)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Set (Set, toUnfoldable)
import Data.Set as Set
import Data.Traversable (traverse_)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, uncons, (:))
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
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrlAddr)
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (EgestServer, PoPName, RelayServer, Server, extractAddress, extractPoP)
import Shared.Types.Agent.State as PublicState
import SpudGun as SpudGun

type CreateRelayPayload
  = { streamId :: StreamId
    , aggregatorPoP :: PoPName
    }

type RegisterRelayChainPayload
  = { streamId :: StreamId
    , aggregatorPoP :: PoPName
    , route :: RelayRoute
    }


type RegisterEgestPayload
  = { streamId :: StreamId
    , aggregatorPoP :: PoPName
    , egestServer :: EgestServer
    }


-- basically a listzipper...
type RelayRoute
  = { toEgest :: List PoPName
    , thisPoP :: PoPName
    , toSource :: List PoPName
    }

type State
  = { streamId :: StreamId
    , aggregatorPoP :: PoPName
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
       , aggregatorPoP : payload.aggregatorPoP
       , thisServer
       , relaysServed : mempty
       , egestsServed : mempty
       , egestSourceRoutes : Nothing
       }

registerEgest :: RegisterEgestPayload -> Effect Unit
registerEgest payload = Gen.doCall (serverName payload.streamId) $ doRegisterEgest
  where
    doRegisterEgest :: State -> Effect (CallResult Unit State)
    doRegisterEgest state@{egestsServed} = do
      _ <- logInfo "Register egest " {payload}
      newState <- maybeStartEgestRelays state{ egestsServed = Set.insert payload.egestServer egestsServed}
      pure $ CallReply unit $ spy "registerEgest" newState

registerRelayChain :: RegisterRelayChainPayload -> Effect Unit
registerRelayChain payload = pure unit

maybeStartEgestRelays :: State -> Effect State
maybeStartEgestRelays state@{egestSourceRoutes: Just _} = pure state
maybeStartEgestRelays state@{streamId, aggregatorPoP, thisServer} = do
  let
    thisPoP = spy "thisPoP" $ extractPoP thisServer
    toRelayRoute :: List PoPName -> RelayRoute
    toRelayRoute pops = { toSource : pops
                        , thisPoP  : thisPoP
                        , toEgest  : nil
                        }
  relayRoutes <- (map toRelayRoute) <$> TransPoP.routesTo aggregatorPoP
  _ <- traverse_ (walkRelayChain streamId aggregatorPoP) relayRoutes
  pure state{egestSourceRoutes = Just (spy "relayRoutes" relayRoutes)}


walkRelayChain :: StreamId -> PoPName -> RelayRoute -> Effect Unit
walkRelayChain streamId aggregatorPoP {toSource, thisPoP, toEgest} = do
  case uncons toSource of
    Nothing -> pure unit
    Just {head, tail} -> do
      let
        thePayload :: RegisterRelayChainPayload
        thePayload = { streamId
                     , aggregatorPoP
                     , route : { toEgest: (thisPoP : toEgest)
                               , thisPoP: head
                               , toSource: tail
                               }
                     }
      void $ spawnLink (\_ -> notifyNextInChain thePayload)
  where
    notifyNextInChain :: RegisterRelayChainPayload -> Effect Unit
    notifyNextInChain payload@{route: {thisPoP: nextInChain}} = do
      -- TODO - strategy for knowing which servers in a remote PoP are healthy
      -- for now route via the transPoP leader
      mPoPLeader <- TransPoP.getLeaderFor nextInChain
      case mPoPLeader of
        Nothing -> do
          _ <- logWarning "No PoP Leader for relay chain" {nextInChain}
          notifyNextInChain payload
        Just popLeader -> do
          let
            url = makeUrlAddr popLeader RelayChainE
          resp <- SpudGun.postJson url payload
          if isRight resp then
            pure unit
          else do
            _ <- logWarning "Bad return code from post to create relay chain" {nextInChain, resp}
            -- TODO sleep duration from config
            _ <- Erl.sleep (wrap 500)
            notifyNextInChain payload




--  void <$> crashIfLeft =<<



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

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
