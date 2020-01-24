module Rtsv2.Agents.IntraPoP
  ( startLink
  , isStreamIngestAvailable
  , isIngestActive
  , announceLoad
  , announceEgestIsAvailable
  , announceEgestStopped
  , announceStreamIsAvailable
  , announceStreamStopped
  , announceRemoteStreamIsAvailable
  , announceRemoteStreamStopped
  , announceTransPoPLeader
  , whereIsIngestAggregator
  , whereIsStreamRelay
  , whereIsEgest
  , getIdleServer
  , currentTransPoPLeader
  , health
  , bus
  , IntraMessage(..)
  , StreamState(..)
  , EgestState(..)
  , IntraPoPBusMessage(..)
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..), hush)
import Data.Filterable (filterMap)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Ephemeral.Map (EMap)
import Ephemeral.Map as EMap
import Ephemeral.MultiMap (EMultiMap)
import Ephemeral.MultiMap as MultiMap
import Erl.Atom (Atom)
import Erl.Data.List (List, index, length, nil, singleton, sortBy, take, uncons)
import Erl.Data.Map (Map, alter, fromFoldable, values)
import Erl.Data.Map as Map
import Erl.Process (Process, spawnLink)
import Erl.Utils (Milliseconds)
import Erl.Utils as Erl
import Logger (Logger)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub, class Union)
import Record as Record
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Health (Health, percentageToHealth)
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (IpAndPort, LamportClock)
import Serf as Serf
import Shared.Stream (StreamId(..), StreamAndVariant(..))
import Shared.Types (Load, Server(..), ServerAddress(..), ServerLoad(..), extractAddress, toServer, toServerLoad)

type MemberInfo = { serfMember :: Serf.SerfMember
                  , load :: Load
                  , server :: Server
                  }

type LamportClocks =
  { streamStateClocks :: Map ServerAddress LamportClock
  , egestStateClocks :: Map ServerAddress LamportClock
  , loadClocks :: Map ServerAddress LamportClock
  , popLeaderClocks :: Map ServerAddress LamportClock
  }

type State
  = { transPoPApi :: Config.TransPoPAgentApi
    , config :: Config.IntraPoPAgentConfig
    , serfRpcAddress :: IpAndPort
    , currentTransPoPLeader :: Maybe ServerAddress
    , egestLocations :: EMultiMap StreamId Server
    , streamRelayLocations :: EMap StreamId Server
    , streamAggregatorLocations :: EMap StreamId Server
    , thisServer :: Server
    , load :: Load
    , members :: Map ServerAddress MemberInfo
    , expireThreshold :: Milliseconds
    , lamportClocks :: LamportClocks
    }

data StreamState = StreamAvailable
                 | StreamStopped

data EgestState = EgestAvailable
                | EgestStopped

data IntraMessage = IMStreamState StreamState StreamId ServerAddress
                  | IMEgestState EgestState StreamId ServerAddress
                  | IMServerLoad ServerAddress Load
                  | IMTransPoPLeader ServerAddress

data Msg
  = JoinAll
  | GarbageCollect
  | IntraPoPSerfMsg (Serf.SerfMessage IntraMessage)

data IntraPoPBusMessage =
  IngestAggregatorExited StreamId Server

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
health :: Effect Health
health =
  Gen.doCall serverName \state@{ members } -> do
    allOtherServers <- PoPDefinition.getOtherServersForThisPoP
    let
      currentHealth = percentageToHealth $ (Map.size members) / ((length allOtherServers) + 1) * 100
    pure $ CallReply currentHealth state

bus :: Bus.Bus IntraPoPBusMessage
bus = Bus.bus "intrapop_bus"

isStreamIngestAvailable :: StreamId -> Effect Boolean
isStreamIngestAvailable streamId =
  Gen.call serverName \state@{ streamAggregatorLocations } ->
    CallReply (EMap.member streamId streamAggregatorLocations) state

isIngestActive :: StreamAndVariant -> Effect Boolean
isIngestActive (StreamAndVariant s v) = Gen.call serverName \state -> CallReply false state

whereIsIngestAggregator :: StreamId -> Effect (Maybe Server)
whereIsIngestAggregator (StreamId streamId) =
  Gen.call serverName \state ->
    CallReply (EMap.lookup (StreamId streamId) state.streamAggregatorLocations) state

whereIsStreamRelay :: StreamId -> Effect (Maybe Server)
whereIsStreamRelay streamId =
  Gen.call serverName \state ->
    CallReply (EMap.lookup streamId state.streamRelayLocations) state

whereIsEgest :: StreamId -> Effect (List ServerLoad)
whereIsEgest streamId =
  Gen.call serverName
  \state@{egestLocations, members} ->
    let withLoad (Server el) = serverLoad <$> Map.lookup el.address members
    in
    CallReply (filterMap withLoad $ MultiMap.lookup streamId state.egestLocations) state

--------------------------------------------------------------------------------
-- TODO re-read Power of Two articles such as
-- https://www.nginx.com/blog/nginx-power-of-two-choices-load-balancing-algorithm/
-- TODO - predicate should probable be ServerLoad -> Weighting
--------------------------------------------------------------------------------
getIdleServer :: (ServerLoad -> Boolean) -> Effect (Maybe ServerLoad)
getIdleServer pred = Gen.doCall serverName
  (\state@{thisServer, members, load} -> do
      let thisServerLoad = toServerLoad thisServer load
      if pred thisServerLoad
      then pure $ CallReply (Just thisServerLoad) state
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
                pure Nothing
              1 ->
                case uncons nMostIdleWithCapacity of
                  Just {head} -> pure $ Just head
                  Nothing -> pure Nothing
              _ ->
                do
                  -- TODO - always seems to give the same answer!
                  chosenIndex <- randomInt 0 (numServers - 1)
                  pure $ index nMostIdleWithCapacity chosenIndex
          pure $ CallReply resp state
  )


currentTransPoPLeader :: Effect (Maybe ServerAddress)
currentTransPoPLeader =
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

-- Called by EgestAgent to indicate egest on this node
announceEgestIsAvailable :: StreamId -> Effect Unit
announceEgestIsAvailable streamId =
  Gen.doCast serverName
    \state@{ egestLocations, thisServer } -> do
      newEgestLocations <- MultiMap.insert' streamId thisServer egestLocations
      _ <- logInfo "Local egest available" { streamId: streamId }
      _ <- sendToIntraSerfNetwork state "egestAvailable" (IMEgestState EgestAvailable streamId $ extractAddress thisServer)
      pure $ Gen.CastNoReply state { egestLocations = newEgestLocations }

-- Called by EgestAgent to indicate egest on this node has stopped
announceEgestStopped :: StreamId -> Effect Unit
announceEgestStopped streamId =
  Gen.doCast serverName
    \state@{ egestLocations, thisServer } -> do
      let
        newEgestLocations = MultiMap.delete streamId thisServer egestLocations
      _ <- logInfo "Local egest stopped" {streamId}
      _ <- sendToIntraSerfNetwork state "egestStopped" $ IMEgestState EgestStopped streamId $ extractAddress thisServer
      pure $ Gen.CastNoReply state { egestLocations = newEgestLocations }

-- Called by IngestAggregator to indicate stream on this node
announceStreamIsAvailable :: StreamId -> Effect Unit
announceStreamIsAvailable streamId =
  Gen.doCast serverName
    \state@{ streamAggregatorLocations, thisServer, transPoPApi:{announceStreamIsAvailable: transPoP_announceStreamIsAvailable} } -> do
      logIfNew streamId streamAggregatorLocations "New local stream is available" {streamId}
      _ <- sendToIntraSerfNetwork state "streamAvailable" (IMStreamState StreamAvailable streamId (extractAddress thisServer))
      _ <- transPoP_announceStreamIsAvailable streamId thisServer

      Gen.CastNoReply <$> insertStreamAggregator streamId thisServer state

insertStreamAggregator :: StreamId -> Server -> State -> Effect State
insertStreamAggregator streamId server state@{ streamAggregatorLocations } =
  do
    newStreamAggregatorLocations <- EMap.insert' streamId server streamAggregatorLocations
    pure $ state { streamAggregatorLocations = newStreamAggregatorLocations }

-- Called by IngestAggregator to indicate stream stopped on this node
announceStreamStopped :: StreamId -> Effect Unit
announceStreamStopped streamId =
  Gen.doCast serverName
    \state@{ streamAggregatorLocations, thisServer, transPoPApi:{announceStreamStopped: transPoP_announceStreamStopped} } -> do
      _ <- logInfo "Local stream stopped" {streamId}
      _ <- Bus.raise bus (IngestAggregatorExited streamId thisServer)
      _ <- sendToIntraSerfNetwork state "streamStopped" (IMStreamState StreamStopped streamId (extractAddress thisServer))
      _ <- transPoP_announceStreamStopped streamId thisServer

      let
        newStreamAggregatorLocations = EMap.delete streamId streamAggregatorLocations
      pure $ Gen.CastNoReply state { streamAggregatorLocations = newStreamAggregatorLocations }

-- Called by TransPoP to indicate stream that is present on a node in another PoP
announceRemoteStreamIsAvailable :: StreamId -> Server -> Effect Unit
announceRemoteStreamIsAvailable streamId server =
  Gen.doCast serverName
    \state@{ streamAggregatorLocations } -> do
      logIfNew streamId streamAggregatorLocations "New remote stream is avaiable" {streamId}
      _ <- sendToIntraSerfNetwork state "streamAvailable" (IMStreamState StreamAvailable streamId (extractAddress server))

      Gen.CastNoReply <$> insertStreamAggregator streamId server state

-- Called by TransPoP to indicate stream that has stopped on a node in another PoP
announceRemoteStreamStopped :: StreamId -> Server -> Effect Unit
announceRemoteStreamStopped streamId server =
  Gen.doCast serverName
    \state@{ streamAggregatorLocations } -> do
      _ <- logInfo "Remote stream has stopped" {streamId}
      -- TODO - serverAddress or server for bus message???
      _ <- Bus.raise bus (IngestAggregatorExited streamId server)
      _ <- sendToIntraSerfNetwork state "streamStopped" (IMStreamState StreamStopped streamId (extractAddress server))
      let
        newStreamAggregatorLocations = EMap.delete streamId streamAggregatorLocations
      pure $ Gen.CastNoReply state { streamAggregatorLocations = newStreamAggregatorLocations }

-- Called by TransPoP to indicate that it is acting as this PoP's leader
announceTransPoPLeader :: Effect Unit
announceTransPoPLeader =
  Gen.doCast serverName
    \state@{ thisServer, transPoPApi:{handleRemoteLeaderAnnouncement: transPoP_announceTransPoPLeader} } ->
    do
      _ <- transPoP_announceTransPoPLeader thisServer
      _ <- sendToIntraSerfNetwork state "transPoPLeader" (IMTransPoPLeader $ extractAddress thisServer)
      pure $ Gen.CastNoReply state



--------------------------------------------------------------------------------
-- Gen Server methods
--------------------------------------------------------------------------------
startLink :: {config :: Config.IntraPoPAgentConfig, transPoPApi :: Config.TransPoPAgentApi} -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

init :: {config :: Config.IntraPoPAgentConfig, transPoPApi :: Config.TransPoPAgentApi} -> Effect State
init { config: config@{rejoinEveryMs
                      , expireThresholdMs
                      , expireEveryMs}
     , transPoPApi}
               = do
  _ <- logInfo "Intra-PoP Agent Starting" {config: config}
  _ <- Gen.registerExternalMapping serverName (\m -> IntraPoPSerfMsg <$> (Serf.messageMapper m))
  _ <- Timer.sendAfter serverName 0 JoinAll
  _ <- Timer.sendEvery serverName rejoinEveryMs JoinAll
  _ <- Timer.sendEvery serverName expireEveryMs GarbageCollect
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
           _ <- logInfo "Could not connect to IntraPoP Serf Agent" { error: error }
           _ <- Erl.sleep (wrap 100) -- Just so we don't spin like crazy...
           unsafeCrashWith ("could_not_connect_stream")
         Right r ->
           pure r

  serversInPoP <- PoPDefinition.serversInThisPoPByAddress

  let
    busy = wrap 100.0 :: Load
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
    , transPoPApi
    , serfRpcAddress
    , egestLocations: MultiMap.empty
    , streamRelayLocations: EMap.empty
    , streamAggregatorLocations: EMap.empty
    , thisServer: thisServer
    , currentTransPoPLeader: Nothing
    , members
    , load: wrap 0.0
    , expireThreshold: wrap expireThresholdMs
    , lamportClocks: { streamStateClocks: Map.empty
                     , egestStateClocks: Map.empty
                     , loadClocks: Map.empty
                     , popLeaderClocks: Map.empty
                     }
    }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = case msg of
  JoinAll -> do
    _ <- joinAllSerf state
    pure $ CastNoReply state

  GarbageCollect ->
    CastNoReply <$> (garbageCollect state)

  IntraPoPSerfMsg imsg ->
    CastNoReply <$> handleIntraPoPSerfMsg imsg state

handleIntraPoPSerfMsg :: (Serf.SerfMessage IntraMessage) -> State -> Effect State
handleIntraPoPSerfMsg imsg state@{ transPoPApi: {handleRemoteLeaderAnnouncement}
                                 , thisServer
                                 , lamportClocks
                                 } =
  case imsg of
    Serf.MemberAlive members -> membersAlive members state
    Serf.MemberLeaving -> pure state
    Serf.MemberLeft members -> membersLeft members state
    Serf.MemberFailed -> pure state

    Serf.StreamFailed -> do
      _ <- logInfo "Lost connection to IntraPoP Serf Agent" {}
      _ <- Erl.sleep (wrap 100) -- Just so we don't spin like crazy...
      unsafeCrashWith ("lost_serf_connection")

    Serf.UserEvent name ltime coalesce intraMessage -> do
     case intraMessage of
       IMStreamState stateChange streamId address -> do
         mTuple <- screenOriginAndMessageClock thisServer address ltime lamportClocks.streamStateClocks
         case mTuple of
           Nothing -> pure state
           Just (Tuple newClockState server) ->
             handleStreamStateChange stateChange streamId server (state {lamportClocks = lamportClocks {streamStateClocks = newClockState}})

       IMEgestState stateChange streamId address -> do
         mTuple <- screenOriginAndMessageClock thisServer address ltime lamportClocks.egestStateClocks
         case mTuple of
           Nothing -> pure state
           Just (Tuple newClockState server) ->
             handleEgestStateChange stateChange streamId server (state {lamportClocks = lamportClocks {egestStateClocks = newClockState}})

       IMServerLoad address load -> do
         mTuple <- screenOriginAndMessageClock thisServer address ltime lamportClocks.loadClocks
         case mTuple of
           Nothing -> pure state
           Just (Tuple newClockState server) ->
             let
               newMembers = alter (map (\ memberInfo -> memberInfo { load = load })) address state.members
             in
               pure state{ members = newMembers
                         , lamportClocks = lamportClocks {loadClocks = newClockState}
                         }

       IMTransPoPLeader address -> do
         mTuple <- screenOriginAndMessageClock thisServer address ltime lamportClocks.popLeaderClocks
         case mTuple of
           Nothing -> pure state
           Just (Tuple newClockState server) -> do
             _ <- handleRemoteLeaderAnnouncement server
             pure state { currentTransPoPLeader = Just address
                        , lamportClocks = lamportClocks {popLeaderClocks = newClockState}
                        }

handleStreamStateChange :: StreamState -> StreamId -> Server -> State -> Effect State
handleStreamStateChange streamState streamId server state =
  case streamState of
    StreamAvailable -> do
      -- streamAvailable on some other node in this PoP - we need to tell Trans-PoP
      --_ <- logInfo "StreamAvailable on remote node" { streamId: streamId, remoteNode: address }
      _ <- state.transPoPApi.announceStreamIsAvailable streamId server
      _ <- logIfNew streamId state.streamAggregatorLocations "StreamAvailable on remote node" { streamId: streamId, remoteNode: server }

      insertStreamAggregator streamId server state

    StreamStopped -> do
      -- streamStopped on some other node in this PoP - we need to tell Trans-PoP
      -- TODO - we do this in a bunch of places
      _ <- Bus.raise bus $ IngestAggregatorExited streamId server
      _ <- logInfo "StreamStopped on remote node" { streamId: streamId, remoteNode: server }
      _ <- state.transPoPApi.announceStreamStopped streamId server
      let
        newStreamAggregatorLocations = EMap.delete streamId state.streamAggregatorLocations
      pure $ state { streamAggregatorLocations = newStreamAggregatorLocations }

handleEgestStateChange :: EgestState -> StreamId -> Server -> State -> Effect State
handleEgestStateChange stateChange streamId server state =
  case stateChange of
    EgestAvailable -> do
        -- egestAvailable on some other node in this PoP
        _ <- logInfo "EgestAvailable on remote node" { streamId: streamId, remoteNode: server }
        newEgestLocations <- MultiMap.insert' streamId server state.egestLocations
        pure $ state { egestLocations = newEgestLocations }

    EgestStopped -> do
        _ <- logInfo "EgestStopped on remote node" { streamId: streamId, remoteNode: server }
        let
          newEgestLocations = MultiMap.delete streamId server state.egestLocations
        pure $ state { egestLocations = newEgestLocations }


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
  _ <- logInfo "Members Alive" { members: _.name <$> members }

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
  _ <- logInfo "Members Left" { members: _.name <$> members }
  let
    newMembers = foldl (\acc { name } -> Map.delete (ServerAddress name) acc) state.members members
  pure state { members = newMembers }

garbageCollect :: State -> Effect State
garbageCollect state@{ expireThreshold
                     , streamAggregatorLocations
                     , streamRelayLocations
                     , egestLocations
                     } =
  do
    now <- Erl.systemTimeMs
    let threshold = now - expireThreshold
        Tuple newStreamAggregatorLocations aggregatorGarbage = EMap.garbageCollect2 threshold streamAggregatorLocations
        newStreamRelayLocations = EMap.garbageCollect threshold streamRelayLocations
        newEgestLocations = MultiMap.garbageCollect threshold egestLocations
    _ <- traverse (\(Tuple streamId aggregator) -> Bus.raise bus (IngestAggregatorExited streamId aggregator)) aggregatorGarbage
    pure state{ streamAggregatorLocations = newStreamAggregatorLocations
              , streamRelayLocations = newStreamRelayLocations
              , egestLocations = newEgestLocations
              }

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
                  result <- Serf.join serfRpcAddress (singleton $ serverAddressToSerfAddress addr) true
                  _ <- maybeLogError "Intra-PoP serf join failed" result {}
                  pure unit
              )
        traverse_ joinAsync toJoin
  where
  toMap :: forall a. List a -> Map a Unit
  toMap list = foldl (\acc item -> Map.insert item unit acc) Map.empty list


screenOriginAndMessageClock :: Server -> ServerAddress -> LamportClock -> Map ServerAddress LamportClock -> Effect (Maybe (Tuple (Map ServerAddress LamportClock) Server))
screenOriginAndMessageClock (Server thisServer) messageServerAddress msgClock lastClockByServer =
  if Map.lookup messageServerAddress lastClockByServer # maybe false (_ >= msgClock)
  then do
    pure Nothing
  else
    if thisServer.address == messageServerAddress then
      pure Nothing
    else do
      mLocation <- PoPDefinition.whereIsServer messageServerAddress
      case mLocation of
        Nothing -> do
          pure Nothing
        Just location ->
          pure $ Just $ Tuple (Map.insert messageServerAddress msgClock lastClockByServer) $ toServer messageServerAddress location


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
