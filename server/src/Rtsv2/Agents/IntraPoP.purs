module Rtsv2.Agents.IntraPoP
  ( startLink

    -- This data is essentially global - within and across PoPs
  , announceLocalAggregatorIsAvailable
  , announceLocalAggregatorStopped
  , announceOtherPoPAggregatorIsAvailable
  , announceOtherPoPAggregatorStopped


    -- Data scoped to this PoP
  , announceLocalEgestIsAvailable
  , announceLocalEgestStopped
  , announceLocalRelayIsAvailable

  , announceLoad
  , announceTransPoPLeader


    -- State queries

  , getPublicState

  , whereIsIngestAggregator
  , whereIsStreamRelay
  , whereIsEgest
  , getIdleServer
  , getCurrentTransPoPLeader

  , isIngestActive


    -- Helper - not sure it's used... -- TODO
  , launchLocalOrRemoteGeneric

    -- Test helper
  , testHelper
  , health
  , bus
--  , IntraMessage(..)
--  , EventType(..)
  , IntraPoPBusMessage(..)

  , AgentClock
  , TestHelperPayload
  ) where


import Prelude

import Bus as Bus
import Data.Either (Either(..), hush)
import Data.Filterable (filterMap)
import Data.Foldable (foldM, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Random (randomInt)
import Ephemeral.Map (EMap)
import Ephemeral.Map as EMap
import Erl.Atom (Atom)
import Erl.Data.List (List, head, index, length, nil, singleton, sortBy, take, uncons, (:))
import Erl.Data.Map (Map, alter, fromFoldable, values)
import Erl.Data.Map as Map
import Erl.Process (Process, spawnLink)
import Erl.Utils (Ref, makeRef)
import Erl.Utils as Erl
import Logger (Logger, spy)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import PintoHelper (exposeState)
import Prim.Row (class Nub, class Union)
import Record as Record
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), NoCapacity(..), ResourceResp, ServerSelectionPredicate, fromLocalOrRemote)
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Health (Health, percentageToHealth)
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (IpAndPort, LamportClock)
import Serf as Serf
import Shared.Stream (StreamAndVariant(..), StreamId)
import Shared.Types (Load, Milliseconds, Server(..), ServerAddress(..), ServerLoad(..), extractAddress, extractPoP, serverLoadToServer, toServer, toServerLoad)
import Shared.Types.Agent.State as PublicState


type TestHelperPayload =
  { dropAgentMessages :: Boolean
  }


type MemberInfo =
  { serfMember :: Serf.SerfMember
  , load :: Load
  , server :: Server
  }

type ServerClock = Map ServerAddress LamportClock
type AgentClock = Map (Tuple ServerAddress StreamId) LamportClock

type AgentClocks =
  { aggregatorClocks :: AgentClock
  , egestClocks      :: AgentClock
  , relayClocks      :: AgentClock
  }
type ServerClocks =
  { loadClocks       :: ServerClock
  , popLeaderClocks  :: ServerClock
  , vmLivenessClocks :: ServerClock
  }


type AgentLocations = { relays                :: Locations
                      , aggregators           :: Locations
                      , egests                :: Locations
                      }


type Locations = { byStreamId     :: Map StreamId (Set Server)
                 , byRemoteServer :: Map Server (Set StreamId)
                 , remoteTimeouts :: Map (Tuple StreamId Server) Milliseconds
                 }

type State
  = { config                :: Config.IntraPoPAgentConfig
    , healthConfig          :: Config.HealthConfig
    , transPoPApi           :: Config.TransPoPAgentApi
    , serfRpcAddress        :: IpAndPort
    , currentTransPoPLeader :: Maybe Server

    , testDropAgentMessages :: Boolean

    , thisServerRef         :: Ref
    , serverRefs            :: EMap Server Ref

    , thisServer            :: Server
    , load                  :: Load
    , members               :: Map ServerAddress MemberInfo
    , agentClocks           :: AgentClocks
    , agentLocations        :: AgentLocations
    , serverClocks          :: ServerClocks
    }

data EventType
  = Available
  | Stopped

data IntraMessage
  = IMAggregatorState EventType StreamId ServerAddress
  | IMEgestState EventType StreamId ServerAddress
  | IMRelayState EventType StreamId ServerAddress

  | IMServerLoad ServerAddress Load
  | IMTransPoPLeader ServerAddress
  | IMVMLiveness ServerAddress Ref

data Msg
  = JoinAll
  | GarbageCollectVM
  | GarbageCollectAgents
  | IntraPoPSerfMsg (Serf.SerfMessage IntraMessage)
  | VMLiveness
  | ReAnnounce StreamId HandlerName

data IntraPoPBusMessage =
  IngestAggregatorExited StreamId Server



--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
health :: Effect Health
health =
  Gen.doCall serverName \state@{ members, healthConfig } -> do
    allOtherServers <- PoPDefinition.getOtherServersForThisPoP
    let
      currentHealth = percentageToHealth healthConfig $ (Map.size members) * 100 / ((length allOtherServers) + 1)
    pure $ CallReply currentHealth state

bus :: Bus.Bus IntraPoPBusMessage
bus = Bus.bus "intrapop_bus"


testHelper :: TestHelperPayload -> Effect Unit
testHelper payload =
  Gen.doCall serverName \state -> do
    pure $ CallReply unit state {testDropAgentMessages = payload.dropAgentMessages}



getPublicState :: Effect (PublicState.IntraPoP List)
getPublicState = exposeState publicState serverName
  where
    publicState state@{agentLocations, currentTransPoPLeader} =
      { aggregatorLocations: toLocations <$>  Map.toUnfoldable agentLocations.aggregators.byStreamId
      , relayLocations: toLocations <$>  Map.toUnfoldable agentLocations.relays.byStreamId
      , egestLocations: toLocations <$>  Map.toUnfoldable agentLocations.egests.byStreamId
      , currentTransPoPLeader
      }
    toLocations (Tuple k v) = { streamId: k
                              , servers:  Set.toUnfoldable v
                              }


--TODO....
isIngestActive :: StreamAndVariant -> Effect Boolean
isIngestActive (StreamAndVariant s v) = Gen.call serverName \state -> CallReply false state



-- TODO - we should be calling the prime' versions and handling that there might, in fact be more than
-- one instance of things we'd like to be singletons
whereIsIngestAggregator :: StreamId -> Effect (Maybe Server)
whereIsIngestAggregator streamId  = head <$> whereIs (_.aggregators) streamId

whereIsStreamRelay :: StreamId -> Effect (Maybe (LocalOrRemote Server))
whereIsStreamRelay streamId  = head <$> (map $ map serverLoadToServer) <$> whereIsStreamRelay' streamId

whereIsEgest :: StreamId -> Effect (List ServerLoad)
whereIsEgest streamId = (map fromLocalOrRemote) <$> whereIsEgest' streamId

whereIsStreamRelay' :: StreamId -> Effect (List (LocalOrRemote ServerLoad))
whereIsStreamRelay' = whereIsWithLoad _.relays

whereIsEgest' :: StreamId -> Effect (List (LocalOrRemote ServerLoad))
whereIsEgest' = whereIsWithLoad _.egests


whereIs ::(AgentLocations -> Locations) -> StreamId -> Effect (List Server)
whereIs extractMap streamId =
  Gen.call serverName
  \state@{thisServer, members, agentLocations} ->
    let
      streamLocations = _.byStreamId $ extractMap agentLocations
      mLocations = Map.lookup streamId streamLocations :: Maybe (Set Server)
      locations = maybe nil Set.toUnfoldable mLocations
    in
    CallReply locations state


whereIsWithLoad ::(AgentLocations -> Locations) -> StreamId -> Effect (List (LocalOrRemote ServerLoad))
whereIsWithLoad extractMap streamId =
  Gen.call serverName
  \state@{thisServer, members, agentLocations} ->
    let
      streamLocations = _.byStreamId $ extractMap agentLocations
      withLoad (Server el) = serverLoad <$> Map.lookup el.address members
      mLocations = Map.lookup streamId streamLocations :: Maybe (Set Server)

      locations = maybe nil Set.toUnfoldable mLocations
      filtered = filterMap withLoad locations
    in
    CallReply (toLocalOrRemote thisServer <$> filtered) state
  where
    toLocalOrRemote thisSever server =
      if extractAddress thisSever == extractAddress server
      then Local server
      else Remote server


--------------------------------------------------------------------------------
-- TODO re-read Power of Two articles such as
-- https://www.nginx.com/blog/nginx-power-of-two-choices-load-balancing-algorithm/
-- TODO - predicate should probable be ServerLoad -> Weighting
--------------------------------------------------------------------------------
getIdleServer :: ServerSelectionPredicate -> Effect (ResourceResp ServerLoad)
getIdleServer pred = Gen.doCall serverName
  (\state@{thisServer, members, load} -> do
      let thisServerLoad = toServerLoad thisServer load
      if pred thisServerLoad
      then pure $ CallReply (Right $ Local thisServerLoad) state
      else
        let n = 2
            nMostIdleWithCapacity =
              values members
              # filterMap (\memberInfo ->
                            let serverWithLoad = serverLoad memberInfo
                            in if pred serverWithLoad then Just serverWithLoad else Nothing)
              # sortBy (\(ServerLoad sl1) (ServerLoad sl2) -> compare sl1.load sl2.load)
              # take n
            numServers = length nMostIdleWithCapacity
        in do
          resp <-
            case numServers of
              0 ->
                pure $ Left NoCapacity
              1 ->
                case uncons nMostIdleWithCapacity of
                  Just {head} -> pure $ Right $ Remote head
                  Nothing -> pure $ Left NoCapacity
              _ ->
                do
                  -- TODO - always seems to give the same answer!
                  chosenIndex <- randomInt 0 (numServers - 1)
                  pure $ maybe (Left NoCapacity) (Right <<< Remote) $ index nMostIdleWithCapacity chosenIndex
          pure $ CallReply resp state
  )


getCurrentTransPoPLeader :: Effect (Maybe Server)
getCurrentTransPoPLeader =
  Gen.call serverName
    ( \state@{ currentTransPoPLeader: value } ->
        CallReply value state
    )

-- Called by Load to indicate load on this node
announceLoad :: Load -> Effect Unit
announceLoad load =
  Gen.doCast serverName
    \state@{ thisServer, members } -> do
      let
        thisNodeAddress = extractAddress thisServer
        newMembers = alter (map (\ memberInfo -> memberInfo { load = load })) thisNodeAddress members
      _ <- sendToIntraSerfNetwork state "loadUpdate" (IMServerLoad thisNodeAddress load)
      pure $ Gen.CastNoReply state { members = newMembers , load = load}


type AgentMessageHandler = State -> StreamId -> Server -> Effect Unit

type SetAgentClock = AgentClock -> AgentClocks -> AgentClocks
type AgentClockLens = { get :: AgentClocks -> AgentClock
                      , set :: AgentClock -> AgentClocks -> AgentClocks
                      }

type AgentLocationLens = { get :: AgentLocations -> Locations
                         , set :: Locations -> AgentLocations -> AgentLocations
                         }
newtype HandlerName = HandlerName String
derive instance newtypeHandlerName :: Newtype HandlerName _

type AgentHandler =
  { name              :: HandlerName

  , availableLocal    :: AgentMessageHandler
  , availableThisPoP  :: AgentMessageHandler
  , availableOtherPoP :: AgentMessageHandler
  , stoppedLocal      :: AgentMessageHandler
  , stoppedThisPoP    :: AgentMessageHandler
  , stoppedOtherPoP   :: AgentMessageHandler
  , gcThisPoP         :: AgentMessageHandler
  , gcOtherPoP        :: AgentMessageHandler

  , clockLens         :: AgentClockLens
  , locationLens      :: AgentLocationLens

  , reannounceEveryMs :: (State -> Milliseconds)
    -- TODO -- Maybe add isSingleton - to generate warnings if the sets have more than one entry?
  }


type SetServerClock = ServerClock -> ServerClocks -> ServerClocks
type ServerClockLens = { get :: ServerClocks -> ServerClock
                       , set :: ServerClock -> ServerClocks -> ServerClocks
                       }

type ServerMessageHandler =
  { name          :: HandlerName
  , clockLens     :: ServerClockLens
  , handleMessage :: Server -> State -> Effect State
  }

vmLivenessHandler :: Ref -> ServerMessageHandler
vmLivenessHandler ref =
  { name : HandlerName "vmLiveness"
  , clockLens : clockLens
  , handleMessage : handleMessage
  }
  where
    clockLens = { get : _.vmLivenessClocks
                , set : \newClocks serverClocks -> serverClocks{vmLivenessClocks = newClocks}
                }

    handleMessage :: Server -> State -> Effect State
    handleMessage server state = do
      newState <- case EMap.lookup server state.serverRefs of
        Nothing ->
          pure state
        Just curRef
          | ref == curRef ->
            pure state
          | otherwise ->
            garbageCollectServer state server
      newServerRefs <- EMap.insert' server ref state.serverRefs
      pure newState { serverRefs = newServerRefs }

popLeaderHandler :: ServerMessageHandler
popLeaderHandler =
  { name : HandlerName "popLeader"
  , clockLens : clockLens
  , handleMessage : handleMessage
  }
  where
    clockLens = { get : _.popLeaderClocks
                , set : \newClocks serverClocks -> serverClocks{popLeaderClocks = newClocks}
                }

    handleMessage server state = do
      state.transPoPApi.handleRemoteLeaderAnnouncement server
      pure state{ currentTransPoPLeader = Just server }

loadHandler :: Load -> ServerMessageHandler
loadHandler load =
  { name : HandlerName "load"
  , clockLens : clockLens
  , handleMessage : handleMessage
  }
  where
    clockLens = { get : _.loadClocks
                , set : \newClocks serverClocks -> serverClocks{loadClocks = newClocks}
                }

    handleMessage server state = do
      let
        newMembers = alter (map (\ memberInfo -> memberInfo { load = load })) (extractAddress server) state.members
      pure state{ members = newMembers }


aggregatorHandler :: AgentHandler
aggregatorHandler
  = { name              : HandlerName "aggregator"
    , availableLocal    : availableLocal
    , availableThisPoP  : availableThisPoP
    , availableOtherPoP : availableOtherPoP
    , stoppedLocal      : stoppedLocal
    , stoppedThisPoP    : stoppedThisPoP
    , stoppedOtherPoP   : stoppedOtherPoP
    , gcThisPoP         : gcThisPoP
    , gcOtherPoP        : gcOtherPoP

    , clockLens         : clockLens
    , locationLens      : locationLens

    , reannounceEveryMs : _.config >>> _.reannounceEveryMs >>> _.aggregator >>> wrap
    }
  where
    availableLocal state streamId server = do
      logInfo "Local aggregator available" {streamId}
      sendToIntraSerfNetwork state "aggregatorAvailable" (IMAggregatorState Available streamId (extractAddress server))
      state.transPoPApi.announceAggregatorIsAvailable streamId server

    availableThisPoP state streamId server = do
      logInfo "New aggregator is available in this PoP" {streamId, server}
      state.transPoPApi.announceAggregatorIsAvailable streamId server

    availableOtherPoP state streamId server = do
      logInfo "New aggregator is available in another PoP" {streamId, server}
      sendToIntraSerfNetwork state "aggregatorAvailable" (IMAggregatorState Available streamId (extractAddress server))

    stoppedLocal state streamId server = do
      logInfo "Local aggregator stopped" {streamId}
      Bus.raise bus (IngestAggregatorExited streamId server)
      sendToIntraSerfNetwork state "aggregatorStopped" $ IMAggregatorState Stopped streamId $ extractAddress server
      state.transPoPApi.announceAggregatorStopped streamId server

    stoppedThisPoP state streamId server = do
      logInfo "Remote aggregator stopped in this PoP" {streamId, server}
      Bus.raise bus (IngestAggregatorExited streamId server)
      state.transPoPApi.announceAggregatorStopped streamId server

    stoppedOtherPoP state streamId server = do
      logInfo "Remote aggregator stopped in another PoP" {streamId, server}
      Bus.raise bus (IngestAggregatorExited streamId server)
      sendToIntraSerfNetwork state "aggregatorStopped" (IMAggregatorState Stopped streamId (extractAddress server))

    gcThisPoP state streamId server = do
      logWarning "Remote aggregator timed out" {streamId, server}
      Bus.raise bus (IngestAggregatorExited streamId server)
      -- we don't relay timeout messages - should only happen with stressed network anyway...

    gcOtherPoP state streamId server = do
      logWarning "Aggregator from other pop timed out" {streamId, server}
      Bus.raise bus (IngestAggregatorExited streamId server)
      -- we don't relay timeout messages - should only happen with stressed network anyway...



    clockLens = { get : _.aggregatorClocks
                 , set : \newClocks agentClocks -> agentClocks{aggregatorClocks = newClocks}
                 }

    locationLens = { get : _.aggregators
                   , set : \newLocations state -> state {aggregators = newLocations}
                   }

-- Called by IngestAggregator to indicate aggregator start / stop on this node
announceLocalAggregatorIsAvailable :: StreamId -> Effect Unit
announceLocalAggregatorIsAvailable = announceAvailableLocal aggregatorHandler

announceLocalAggregatorStopped :: StreamId -> Effect Unit
announceLocalAggregatorStopped = announceStoppedLocal aggregatorHandler

-- -- Called by TransPoP to indicate aggregator start / stop on a node in another PoP
announceOtherPoPAggregatorIsAvailable :: StreamId -> Server -> Effect Unit
announceOtherPoPAggregatorIsAvailable = announceAvailableOtherPoP aggregatorHandler

announceOtherPoPAggregatorStopped :: StreamId -> Server -> Effect Unit
announceOtherPoPAggregatorStopped = announceStoppedOtherPoP aggregatorHandler


egestHandler :: AgentHandler
egestHandler
  = { name              : HandlerName "egest"
    , availableLocal    : availableLocal
    , availableThisPoP  : availableThisPoP
    , availableOtherPoP : availableOtherPoP
    , stoppedLocal      : stoppedLocal
    , stoppedThisPoP    : stoppedThisPoP
    , stoppedOtherPoP   : stoppedOtherPoP
    , gcThisPoP         : gcThisPoP
    , gcOtherPoP        : gcOtherPoP

    , clockLens         : clockLens
    , locationLens      : locationLens

    , reannounceEveryMs : _.config >>> _.reannounceEveryMs >>> _.egest >>> wrap

    }
  where
    availableLocal state streamId server = do
      logInfo "Local egest available" {streamId}
      sendToIntraSerfNetwork state "egestAvailable" (IMEgestState Available streamId $ extractAddress server)

    availableThisPoP state streamId server = do
      logInfo "New egest is avaiable in this PoP" {streamId, server}

    availableOtherPoP state streamId server = do
      -- Not expecting any of these
      logWarning "New egest is avaiable in another PoP" {streamId, server}

    stoppedLocal state streamId server = do
      logInfo "Local egest stopped" {streamId}
      sendToIntraSerfNetwork state "egestStopped" $ IMEgestState Stopped streamId $ extractAddress server

    stoppedThisPoP state streamId server = do
      logInfo "Remote egest stopped" {streamId, server}

    stoppedOtherPoP state streamId server = do
      -- Not expecting any of these
      logWarning "Egest stopped in another PoP" {streamId, server}

    gcThisPoP state streamId server = do
      logWarning "Remote egest timed out" {streamId, server}

    gcOtherPoP state streamId server = do
      -- Not expecting any of these
      logWarning "Egest from other pop timed out" {streamId, server}

    clockLens = { get : _.egestClocks
                 , set : \newClocks agentClocks -> agentClocks{egestClocks = newClocks}
                 }

    locationLens = { get : _.egests
                    , set : \newLocations state -> state {egests = newLocations}
                    }

-- Called by EgestAgent to indicate egest on this node
announceLocalEgestIsAvailable :: StreamId -> Effect Unit
announceLocalEgestIsAvailable = announceAvailableLocal egestHandler

announceLocalEgestStopped :: StreamId -> Effect Unit
announceLocalEgestStopped = announceStoppedLocal egestHandler

relayHandler :: AgentHandler
relayHandler
  = { name              : HandlerName "relay"
    , availableLocal    : availableLocal
    , availableThisPoP  : availableThisPoP
    , availableOtherPoP : availableOtherPoP
    , stoppedLocal      : stoppedLocal
    , stoppedThisPoP    : stoppedThisPoP
    , stoppedOtherPoP   : stoppedOtherPoP
    , gcThisPoP         : gcThisPoP
    , gcOtherPoP        : gcOtherPoP

    , clockLens         : clockLens
    , locationLens      : locationLens

    , reannounceEveryMs : _.config >>> _.reannounceEveryMs >>> _.relay >>> wrap
    }
  where
    availableLocal state streamId server = do
      logInfo "Local relay available" {streamId}
      sendToIntraSerfNetwork state "relayAvailable" (IMRelayState Available streamId $ extractAddress server)

    availableThisPoP state streamId server = do
      logInfo "New relay is available in this PoP" {streamId, server}

    availableOtherPoP state streamId server = do
      -- Not expecting any of these
      logWarning "New relay is available in another PoP" {streamId, server}

    stoppedLocal state streamId server = do
      logInfo "Local relay stopped" {streamId}
      sendToIntraSerfNetwork state "relayStopped" $ IMRelayState Stopped streamId $ extractAddress server

    stoppedThisPoP state streamId server = do
      logInfo "Remote relay stopped" {streamId, server}

    stoppedOtherPoP state streamId server = do
      -- Not expecting any of these
      logWarning "Relay stopped in another PoP" {streamId, server}

    gcThisPoP state streamId server = do
      logWarning "Remote relay timed out" {streamId, server}

    gcOtherPoP state streamId server = do
      -- Not expecting any of these
      logWarning "Relay from other pop timed out" {streamId, server}


    clockLens = { get : _.relayClocks
                 , set : \newClocks agentClocks -> agentClocks{relayClocks = newClocks}
                 }

    locationLens = { get : _.relays
                    , set : \newLocations state -> state {relays = newLocations}
                    }

-- Called by RelayAgent to indicate relay on this node
announceLocalRelayIsAvailable :: StreamId -> Effect Unit
announceLocalRelayIsAvailable = announceAvailableLocal relayHandler

announceLocalRelayStopped :: StreamId -> Effect Unit
announceLocalRelayStopped = announceStoppedLocal relayHandler


-- Builds public API for events on this server
announceAvailableLocal :: AgentHandler -> StreamId -> Effect Unit
announceAvailableLocal handler@{locationLens} streamId =
  Gen.doCast serverName
    \state@{thisServer} -> do
      doAnnounceAvailableLocal handler streamId state
      pure $ Gen.CastNoReply $ updateAgentLocation recordLocalAgent locationLens streamId thisServer state

doAnnounceAvailableLocal :: AgentHandler -> StreamId -> State -> Effect Unit
doAnnounceAvailableLocal handler@{locationLens} streamId state@{thisServer, agentLocations} = do
  handler.availableLocal state streamId thisServer
  void $ Timer.sendAfter serverName (unwrap $ handler.reannounceEveryMs state) $ ReAnnounce streamId handler.name

--TODO - don't need to update the loaction here - top level function is enough

announceStoppedLocal :: AgentHandler -> StreamId -> Effect Unit
announceStoppedLocal handler@{locationLens} streamId = do
  Gen.doCast serverName
    \state@{ thisServer } -> do
      handler.stoppedLocal state streamId thisServer
      pure $ Gen.CastNoReply $ updateAgentLocation removeLocalAgent locationLens streamId thisServer state



-- Builds public API for message from other PoP
announceAvailableOtherPoP :: AgentHandler -> StreamId -> Server -> Effect Unit
announceAvailableOtherPoP handler@{locationLens} streamId server =
  Gen.doCast serverName
    \state -> do
      timeout <- messageTimeout handler state
      handler.availableOtherPoP state streamId server
      pure $ Gen.CastNoReply $ updateAgentLocation (recordRemoteAgent timeout) locationLens streamId server state

announceStoppedOtherPoP :: AgentHandler -> StreamId -> Server -> Effect Unit
announceStoppedOtherPoP handler@{locationLens} streamId server =
  Gen.doCast serverName
    \state -> do
      handler.stoppedOtherPoP state streamId server
      pure $ Gen.CastNoReply $ updateAgentLocation removeRemoteAgent locationLens streamId server state

-- Called by TransPoP to indicate that it is acting as this PoP's leader
announceTransPoPLeader :: Effect Unit
announceTransPoPLeader =
  Gen.doCast serverName
    \state@{ thisServer, transPoPApi:{handleRemoteLeaderAnnouncement: transPoP_announceTransPoPLeader} } ->
    do
      transPoP_announceTransPoPLeader thisServer
      sendToIntraSerfNetwork state "transPoPLeader" (IMTransPoPLeader $ extractAddress thisServer)
      pure $ Gen.CastNoReply state{currentTransPoPLeader = Just thisServer}


updateAgentLocation :: (StreamId -> Server -> Locations -> Locations) -> AgentLocationLens -> StreamId -> Server -> State -> State
updateAgentLocation action lens streamId server state@{agentLocations} =
  let
    locations = lens.get agentLocations
    newLocations = action streamId server locations
    newAgentLocations = lens.set newLocations agentLocations
  in
    state { agentLocations = newAgentLocations }


--------------------------------------------------------------------------------
-- Helper for consistent behaivour around launching resources within a PoP
--------------------------------------------------------------------------------
launchLocalOrRemoteGeneric :: (ServerLoad -> Boolean) -> (ServerLoad -> Effect Unit) -> (ServerLoad -> Effect Unit) -> Effect (ResourceResp Server)
launchLocalOrRemoteGeneric pred launchLocal launchRemote = do
  idleServerResp <- getIdleServer pred
  launchResp <-  -- TODO - currently unit - maybe allow some sort of check?
    case idleServerResp of
      Right (Local local) ->
        launchLocal local
      Right (Remote remote) ->
        launchRemote remote
      Left NoCapacity ->
        pure unit
  pure $ (map serverLoadToServer) <$> idleServerResp


--------------------------------------------------------------------------------
-- Gen Server methods
--------------------------------------------------------------------------------
startLink :: {config :: Config.IntraPoPAgentConfig, transPoPApi :: Config.TransPoPAgentApi} -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

init :: {config :: Config.IntraPoPAgentConfig, transPoPApi :: Config.TransPoPAgentApi} -> Effect State
init { config: config@{ rejoinEveryMs
                      , checkVMExpiryEveryMs
                      , checkAgentExpiryEveryMs
                      , reannounceEveryMs
                      }
     , transPoPApi}
               = do
  logInfo "Intra-PoP Agent Starting" {config: config}
  healthConfig <- Config.healthConfig
  Gen.registerExternalMapping serverName (\m -> IntraPoPSerfMsg <$> (Serf.messageMapper m))
  void $ Timer.sendAfter serverName 0 JoinAll
  void $ Timer.sendEvery serverName rejoinEveryMs JoinAll
  void $ Timer.sendEvery serverName reannounceEveryMs.vm VMLiveness
  void $ Timer.sendEvery serverName checkVMExpiryEveryMs GarbageCollectVM
  void $ Timer.sendEvery serverName checkAgentExpiryEveryMs GarbageCollectAgents
  rpcBindIp <- Env.privateInterfaceIp
  thisServer <- PoPDefinition.getThisServer
  let
    serfRpcAddress =
      { ip: show rpcBindIp
      , port: config.rpcPort
      }
  membersResp <- Serf.members serfRpcAddress
  streamResp <- Serf.stream serfRpcAddress
  _ <- case streamResp of
         Left error -> do
           logInfo "Could not connect to IntraPoP Serf Agent" { error: error }
           _ <- Erl.sleep (wrap 100) -- Just so we don't spin like crazy...
           unsafeCrashWith ("could_not_connect_stream")
         Right r ->
           pure r

  serversInPoP <- PoPDefinition.serversInThisPoPByAddress
  thisServerRef <- makeRef

  let
    busy = wrap 100.0 :: Load
    emptyLocations = { byStreamId       : Map.empty
                     , byRemoteServer   : Map.empty
                     , remoteTimeouts   : Map.empty
                     }
    members = membersResp
              # hush
              # fromMaybe nil
              # filterMap (\member@{name} ->
                            let
                              serverAddress = ServerAddress name
                              serverToMemberInfo server =
                                { server: server
                                , load: busy
                                , serfMember: member
                                }
                            in
                              map (Tuple serverAddress) $ map serverToMemberInfo $ Map.lookup serverAddress serversInPoP
                          )
              # fromFoldable
  pure
    { config
    , healthConfig
    , transPoPApi
    , serfRpcAddress
    , currentTransPoPLeader: Nothing

    , testDropAgentMessages: false

    , thisServerRef
    , serverRefs: EMap.empty

    , thisServer: thisServer
    , load: wrap 0.0
    , members
    , agentClocks  : { aggregatorClocks: Map.empty
                     , egestClocks: Map.empty
                     , relayClocks: Map.empty
                     }
    , agentLocations : { relays: emptyLocations
                       , aggregators:  emptyLocations
                       , egests :  emptyLocations
                       }

    , serverClocks: { loadClocks: Map.empty
                    , popLeaderClocks: Map.empty
                    , vmLivenessClocks : Map.empty
                    }
    }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = case msg of

  IntraPoPSerfMsg imsg ->
    CastNoReply <$> handleIntraPoPSerfMsg imsg state

  JoinAll -> do
    _ <- joinAllSerf state
    pure $ CastNoReply state

  GarbageCollectVM ->
    CastNoReply <$> garbageCollectVM state

  GarbageCollectAgents ->
    CastNoReply <$> garbageCollectAgents state

  VMLiveness -> do
    sendToIntraSerfNetwork state "vmLiveness" (IMVMLiveness (extractAddress state.thisServer) state.thisServerRef )
    pure $ CastNoReply state

  ReAnnounce streamId handlerName -> do
    CastNoReply <$> maybeReannounce
    where
      maybeReannounce :: Effect State
      maybeReannounce = do
        let
          handler = handlerFor handlerName
          -- Do we still know about the asset? If we do, rebroadcast its existence and set up another timer for next time
          locations = handler.locationLens.get state.agentLocations
        if mapSetMember streamId state.thisServer locations.byStreamId
        then do
          doAnnounceAvailableLocal handler streamId state
          pure state
        else
          pure state

      handlerFor :: HandlerName -> AgentHandler
      handlerFor (HandlerName name) = unsafePartial $
        case name of
          "aggregator" -> aggregatorHandler
          "egest"      -> egestHandler
          "relay"      -> relayHandler




handleIntraPoPSerfMsg :: (Serf.SerfMessage IntraMessage) -> State -> Effect State
handleIntraPoPSerfMsg imsg state@{ transPoPApi: {handleRemoteLeaderAnnouncement}
                                 , thisServer
                                 , serverClocks
                                 , agentClocks
                                 } =
  case imsg of
    Serf.MemberAlive members -> membersAlive members state
    Serf.MemberLeaving -> pure state
    Serf.MemberLeft members -> membersLeft members state
    Serf.MemberFailed -> pure state

    Serf.StreamFailed -> do
      logInfo "Lost connection to IntraPoP Serf Agent" {}
      _ <- Erl.sleep (wrap 100) -- Just so we don't spin like crazy...
      -- TODO send a "stepping down message?" - except without Serf I guess we can't
      unsafeCrashWith ("lost_serf_connection")

    Serf.UserEvent name ltime coalesce intraMessage -> do
      case intraMessage of
        IMAggregatorState eventType streamId msgOrigin -> do
          handleAgentMessage ltime eventType streamId msgOrigin state aggregatorHandler

        IMEgestState eventType streamId msgOrigin -> do
          handleAgentMessage ltime eventType streamId msgOrigin state egestHandler

        IMRelayState eventType streamId msgOrigin -> do
          handleAgentMessage ltime eventType streamId msgOrigin state relayHandler

        IMServerLoad msgOrigin load -> do
          handleServerMessage ltime msgOrigin state $ loadHandler load

        IMTransPoPLeader msgOrigin -> do
          handleServerMessage ltime msgOrigin state popLeaderHandler

        IMVMLiveness msgOrigin ref -> do
          handleServerMessage ltime msgOrigin state $ vmLivenessHandler ref


handleServerMessage :: LamportClock -> ServerAddress -> State -> ServerMessageHandler -> Effect State
handleServerMessage msgLTime msgServerAddress
                    state@{thisServer, serverClocks}
                    serverMessageHandler@{clockLens, handleMessage} = do
  -- let _ = spy "serverMessage" {name: serverMessageHandler.name, msgServerAddress}
  -- Make sure the message is from a known origin and does not have an expired Lamport clock
  if msgServerAddress == extractAddress thisServer
  then
    pure state
  else let serverClock = clockLens.get state.serverClocks in
    if Map.lookup msgServerAddress serverClock # maybe false (_ >= msgLTime)
    then
      pure state
    else do
      -- TODO - maybe cache the db for a while to prevent hot calls, or use ETS etc
      mLocation <- PoPDefinition.whereIsServer msgServerAddress
      case mLocation of
        Nothing -> do
          logWarning "message from unknown server" {msgServerAddress, msgType: serverMessageHandler.name}
          pure state
        Just msgLocation -> do
          let
            msgServer = toServer msgServerAddress msgLocation
            newServerClock = Map.insert msgServerAddress msgLTime serverClock
            newServerClocks = clockLens.set newServerClock state.serverClocks
          handleMessage  msgServer state{ serverClocks = newServerClocks}


handleAgentMessage :: LamportClock -> EventType -> StreamId -> ServerAddress -> State -> AgentHandler -> Effect State
handleAgentMessage msgLTime eventType streamId msgServerAddress
                  state@{thisServer, agentClocks, agentLocations}
                  agentMessageHandler@{clockLens, locationLens} = do
  -- let _ = spy "agentMessage" {name: agentMessageHandler.name, eventType, streamId, msgServerAddress}
  -- Make sure the message is from a known origin and does not have an expired Lamport clock
  if msgServerAddress == extractAddress thisServer || state.testDropAgentMessages
  then
    pure state
  else let agentClock = clockLens.get state.agentClocks in
    if Map.lookup (Tuple msgServerAddress streamId) agentClock # maybe false (_ >= msgLTime)
    then
      pure state
    else do
      -- TODO - maybe cache the db for a while to prevent hot calls, or use ETS etc
      mLocation <- PoPDefinition.whereIsServer msgServerAddress
      case mLocation of
        Nothing -> do
          logWarning "message from unknown server" {msgServerAddress, msgType: agentMessageHandler.name}
          pure state
        Just msgLocation
          | extractPoP msgLocation /= extractPoP state.thisServer ->
            pure state
          | otherwise -> do
            let
              msgServer = toServer msgServerAddress msgLocation
              newAgentClock = Map.insert (Tuple msgServerAddress streamId) msgLTime agentClock
              newAgentClocks = clockLens.set newAgentClock state.agentClocks
              locations = locationLens.get agentLocations

            case eventType of
              Available -> do
                timeout <- messageTimeout agentMessageHandler state
                let
                  newLocations = recordRemoteAgent timeout streamId msgServer locations
                agentMessageHandler.availableThisPoP state streamId msgServer
                pure $ state { agentClocks = newAgentClocks
                             , agentLocations = locationLens.set newLocations agentLocations
                             }

              Stopped -> do
                let
                  newLocations = removeRemoteAgent streamId msgServer locations
                agentMessageHandler.stoppedThisPoP state streamId msgServer
                pure $ state { agentClocks = newAgentClocks
                             , agentLocations = locationLens.set newLocations agentLocations
                             }


messageTimeout :: AgentHandler -> State -> Effect Milliseconds
messageTimeout agentMessageHandler state = do
  now <- Erl.systemTimeMs
  pure $ now + ((agentMessageHandler.reannounceEveryMs state) * (wrap state.config.missCountBeforeExpiry))

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------
serverName :: ServerName State Msg
serverName = Names.intraPoPName

logIfNew :: forall a v. StreamId -> EMap StreamId v -> String -> Record a -> Effect Unit
logIfNew streamId m str metadata =
  if EMap.member streamId m
  then pure unit
  else void $ logInfo str metadata

sendToIntraSerfNetwork :: State -> String -> IntraMessage -> Effect Unit
sendToIntraSerfNetwork state name msg = do
  result <- Serf.event state.serfRpcAddress name msg false
  _ <- maybeLogError "Intra-PoP serf event failed" result {}
  pure unit


membersAlive :: (List Serf.SerfMember) -> State -> Effect State
membersAlive members state = do
  logInfo "Members Alive" { members: _.name <$> members }

  serversInPoP <- PoPDefinition.serversInThisPoPByAddress

  let
    makeMemberInfo member@{ name } server =
      { serfMember: member
      , server: server
      , load: wrap 100.0 :: Load
      }

    newMembers
      = members
        # filterMap (\ member@{ name } ->
                      let
                        address = (ServerAddress name)
                      in
                       Map.lookup address serversInPoP
                       # map (\server -> Tuple address (makeMemberInfo member server))
                    )
        # foldl (\acc (Tuple address memberInfo) -> Map.insert address memberInfo acc) state.members

  pure state { members = newMembers }

membersLeft :: (List Serf.SerfMember) -> State -> Effect State
membersLeft members state = do
  logInfo "Members Left" { members: _.name <$> members }
  let
    newMembers = foldl (\acc { name } -> Map.delete (ServerAddress name) acc) state.members members
  pure state { members = newMembers }

garbageCollectVM :: State -> Effect State
garbageCollectVM state@{ config
                       , serverRefs
                       } =
  do
    now <- Erl.systemTimeMs
    let
      expireThreshold = wrap $ config.missCountBeforeExpiry * config.reannounceEveryMs.vm
      threshold = now - expireThreshold
      Tuple newServerRefs garbageRefs = EMap.garbageCollect2 threshold serverRefs
      garbageServers = fst <$> garbageRefs
    foldM garbageCollectServer state{serverRefs = newServerRefs} garbageServers


garbageCollectAgents :: State -> Effect State
garbageCollectAgents state = do
  now <- Erl.systemTimeMs
  foldHandlers (gcAgent now) state

gcAgent :: Milliseconds -> AgentHandler -> State -> Effect State
gcAgent now handler@{locationLens} state = do
  let
    thisPoP = extractPoP state.thisServer
    locations = locationLens.get state.agentLocations
    garbage = foldlWithIndex expired nil locations.remoteTimeouts
  foldM (gc thisPoP) state garbage
--  pure $ locationLens.set newLocations state
  --key@(Tuple streamId server) acc v = if v < now then (key : acc) else acc
  where
    expired k acc v = if v < now then (k : acc) else acc
    gc thisPoP state' (Tuple streamId server) = do
      if extractPoP server == thisPoP
      then handler.gcThisPoP state' streamId server
      else handler.gcOtherPoP state' streamId server
      pure $ updateAgentLocation removeRemoteAgent handler.locationLens streamId server state'


foldHandlers :: forall acc. (AgentHandler -> acc -> Effect acc) -> acc -> Effect acc
foldHandlers perHandlerFun =
  perHandlerFun relayHandler >=> perHandlerFun aggregatorHandler >=> perHandlerFun egestHandler

garbageCollectServer :: State -> Server -> Effect State
garbageCollectServer state deadServer = do
  -- TODO - add a server decomissioning message to do this cleanly
  logWarning "server liveness timeout" {server: deadServer}
  foldHandlers (gcServer deadServer) state

gcServer :: Server -> AgentHandler -> State -> Effect State
gcServer deadServer agentHandler@{locationLens, name, stoppedThisPoP} state@{agentLocations} = do
  let
    locations = locationLens.get agentLocations
  case Map.lookup deadServer locations.byRemoteServer of
    Nothing -> pure state
    Just garbageStreams -> do
      logWarning (unwrap name <> " liveness timeout") {garbageStreams}
      newByStreamId <- foldM cleanUp locations.byStreamId  garbageStreams
      let
        newTimeouts = foldl (\acc k -> Map.delete (Tuple k deadServer) acc) locations.remoteTimeouts garbageStreams
        newLocations = { byStreamId : newByStreamId
                       , byRemoteServer : Map.delete deadServer locations.byRemoteServer
                       , remoteTimeouts : newTimeouts
                       }
      pure $ state {agentLocations = locationLens.set newLocations agentLocations}
  where
    cleanUp acc streamId = do
      stoppedThisPoP state streamId deadServer
      pure $ mapSetDelete streamId deadServer acc



recordLocalAgent :: StreamId -> Server -> Locations -> Locations
recordLocalAgent streamId thisServer locations =
  locations { byStreamId = mapSetInsert streamId thisServer locations.byStreamId
            }

removeLocalAgent :: StreamId -> Server -> Locations -> Locations
removeLocalAgent streamId thisServer locations =
  locations { byStreamId = mapSetDelete streamId thisServer locations.byStreamId
            }


recordRemoteAgent :: Milliseconds -> StreamId -> Server -> Locations -> Locations
recordRemoteAgent timeout streamId server locations =
  { byStreamId     : mapSetInsert streamId server locations.byStreamId
  , byRemoteServer : mapSetInsert server streamId locations.byRemoteServer
  , remoteTimeouts : Map.insert (Tuple streamId server) timeout locations.remoteTimeouts
  }

removeRemoteAgent :: StreamId -> Server -> Locations -> Locations
removeRemoteAgent streamId server locations =
  { byStreamId     : mapSetDelete streamId server locations.byStreamId
  , byRemoteServer : mapSetDelete server streamId locations.byRemoteServer
  , remoteTimeouts : Map.delete(Tuple streamId server) locations.remoteTimeouts
  }

mapSetInsert :: forall k v. Ord k => Ord v => k -> v -> Map k (Set v) -> Map k (Set v)
mapSetInsert k v mapSet =
  let
    current = fromMaybe Set.empty $ Map.lookup k mapSet
  in
    Map.insert k (Set.insert v current) mapSet

mapSetDelete :: forall k v. Ord k => Ord v => k -> v -> Map k (Set v) -> Map k (Set v)
mapSetDelete k v mapSet =
  case Map.lookup k mapSet of
    Nothing -> mapSet
    Just existing ->
      let
        newSet = Set.delete v existing
      in
       if Set.isEmpty newSet
       then Map.delete k mapSet
       else Map.insert k newSet mapSet

mapSetMember :: forall k v. Ord k => Ord v => k -> v -> Map k (Set v) -> Boolean
mapSetMember k v ms = fromMaybe false $ Set.member v <$> Map.lookup k ms


joinAllSerf :: State -> Effect Unit
joinAllSerf { config, serfRpcAddress, members } =
  let
    serverAddressToSerfAddress (ServerAddress s) =
      { ip: s
      , port: config.bindPort
      }
  in
    do
      allOtherServers <- PoPDefinition.getOtherServersForThisPoP
      let
        toJoin = Map.keys $ Map.difference (toMap allOtherServers) members
      if length toJoin < (length allOtherServers) / 2 then
        -- We already know about more than 1/2 the network - no need to issue any joins
        pure unit
      else do
        let
          joinAsync :: ServerAddress -> Effect (Process Unit)
          joinAsync addr =
            spawnLink
              ( \_ -> do
                  result <- Serf.join serfRpcAddress (singleton $ serverAddressToSerfAddress addr) config.replayMessagesOnJoin

                  _ <- maybeLogError "Intra-PoP serf join failed" result {}
                  pure unit
              )
        traverse_ joinAsync toJoin
  where
  toMap :: forall a. List a -> Map a Unit
  toMap list = foldl (\acc item -> Map.insert item unit acc) Map.empty list

serverLoad :: MemberInfo -> ServerLoad
serverLoad {server, load} = toServerLoad server load


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = serverName # Names.toDomain # singleton

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains

maybeLogError :: forall a b c d e. Union b (error :: e) c => Nub c d => String -> Either e a -> Record b  -> Effect Unit
maybeLogError _ (Right _) _ = pure unit
maybeLogError msg (Left err) metadata = do
  _ <- logInfo msg (Record.merge metadata {error: err})
  pure unit
