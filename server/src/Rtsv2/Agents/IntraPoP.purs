module Rtsv2.Agents.IntraPoP
  ( startLink
  , isIngestAvailable
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
  , IntraMessage(..)
  , StreamState(..)
  , EgestState(..)
  ) where

import Prelude

import Data.Either (Either(..), hush)
import Data.Filterable (filterMap)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random (randomInt)
import Ephemeral.Map (EMap)
import Ephemeral.Map as EMap
import Ephemeral.MultiMap (EMultiMap)
import Ephemeral.MultiMap as MultiMap
import Erl.Atom (Atom)
import Erl.Data.List (List, head, index, length, nil, singleton, sortBy, take, uncons)
import Erl.Data.Map (Map, alter, fromFoldable, toUnfoldable, values)
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
import Serf (IpAndPort)
import Serf as Serf
import Shared.Stream (StreamId(..), StreamAndVariant(..))
import Shared.Types (Load, ServerAddress(..), ServerLoad(..), LocatedServer(..))
import Shared.Utils (distinctRandomNumbers)

type State
  = { transPoPApi :: Config.TransPoPAgentApi
    , config :: Config.IntraPoPAgentConfig
    , serfRpcAddress :: IpAndPort
    , currentTransPoPLeader :: Maybe ServerAddress
    , egestLocations :: EMultiMap StreamId ServerAddress
    , streamRelayLocations :: EMap StreamId ServerAddress
    , streamAggregatorLocations :: EMap StreamId LocatedServer
    , thisNode :: ServerAddress
    , members :: Map ServerAddress (Tuple Serf.SerfMember Load)
    , expireThreshold :: Milliseconds
    , streamStateClocks :: EMap StreamId Int
    , egestStateClocks :: EMap StreamId Int
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


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
health :: Effect Health
health =
  Gen.doCall serverName \state@{ members } -> do
    allOtherServers <- PoPDefinition.getOtherServersForThisPoP
    let
      currentHealth = percentageToHealth $ (Map.size members * 100) / (length allOtherServers) * 100
    pure $ CallReply currentHealth state

isIngestAvailable :: StreamId -> Effect Boolean
isIngestAvailable streamId =
  Gen.call serverName \state@{ streamAggregatorLocations } ->
    CallReply (EMap.member streamId streamAggregatorLocations) state

isIngestActive :: StreamAndVariant -> Effect Boolean
isIngestActive (StreamAndVariant s v) = Gen.call serverName \state -> CallReply false state

whereIsIngestAggregator :: StreamId -> Effect (Maybe LocatedServer)
whereIsIngestAggregator (StreamId streamId) =
  Gen.call serverName \state ->
    CallReply (EMap.lookup (StreamId streamId) state.streamAggregatorLocations) state

whereIsStreamRelay :: StreamId -> Effect (Maybe ServerAddress)
whereIsStreamRelay streamId =
  Gen.call serverName \state ->
    CallReply (EMap.lookup streamId state.streamRelayLocations) state

whereIsEgest :: StreamId -> Effect (List ServerLoad)
whereIsEgest streamId =
  Gen.call serverName
  \state@{egestLocations, members} ->
    let withLoad :: ServerAddress -> Maybe ServerLoad
        withLoad el = (ServerLoad el <$>  (snd <$> Map.lookup el members))
    in
    CallReply (filterMap withLoad $ MultiMap.lookup streamId state.egestLocations) state


--------------------------------------------------------------------------------
-- TODO re-read Power of Two articles such as
-- https://www.nginx.com/blog/nginx-power-of-two-choices-load-balancing-algorithm/
--------------------------------------------------------------------------------
getIdleServer :: (Load -> Boolean) -> Effect (Maybe ServerAddress)
getIdleServer pred = Gen.doCall serverName
  (\state@{members} ->
    let n = 2
        nMostIdleWithCapacity =
          toUnfoldable members
          # filterMap (\(Tuple addr (Tuple _ load)) -> if pred load then Just (ServerLoad addr load) else Nothing)
          # sortBy (\(ServerLoad _ l1) (ServerLoad _ l2) -> compare l1 l2)
          <#> (\(ServerLoad addr _) -> addr)
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
              chosenIndex <- randomInt 1 numServers
              pure $ index nMostIdleWithCapacity chosenIndex
      pure $ CallReply resp state
  )



getIdleServer' :: Effect (Maybe ServerAddress)
getIdleServer' = Gen.doCall serverName
  (\state@{members} ->
    let
      membersList = values members
    in
      do
        indexes <- distinctRandomNumbers 1 (min 2 ((length membersList) - 1))
        let
          output = traverse (\i -> index membersList i) indexes
                   # fromMaybe nil
                   # sortBy (\(Tuple _ lhsLoad) (Tuple _ rhsLoad) -> compare lhsLoad rhsLoad)
                   # head
                   <#> fst >>> _.name >>> wrap
        pure $ CallReply output state
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
    \state@{ thisNode, members } -> do
      _ <- sendToIntraSerfNetwork state "loadUpdate" (IMServerLoad thisNode load)
      let
        newMembers = alter (map (\(Tuple member oldLoad) -> Tuple member load)) thisNode members
      pure $ Gen.CastNoReply state { members = newMembers }

-- Called by EgestAgent to indicate egest on this node
announceEgestIsAvailable :: StreamId -> Effect Unit
announceEgestIsAvailable streamId =
  Gen.doCast serverName
    \state@{ egestLocations, thisNode } -> do
      _ <- logInfo "Local egest available" { streamId: streamId }
      _ <- sendToIntraSerfNetwork state "egestAvailable" (IMEgestState EgestAvailable streamId thisNode)
      newEgestLocations <- MultiMap.insert' streamId thisNode  state.egestLocations
      pure $ Gen.CastNoReply state { egestLocations = newEgestLocations }

-- Called by EgestAgent to indicate egest on this node has stopped
announceEgestStopped :: StreamId -> Effect Unit
announceEgestStopped streamId =
  Gen.doCast serverName
    \state@{ egestLocations, thisNode } -> do
      _ <- logInfo "Local egest stopped" {streamId}
      _ <- sendToIntraSerfNetwork state "egestStopped" (IMEgestState EgestStopped streamId thisNode)
      let
        newEgestLocations = MultiMap.delete streamId thisNode egestLocations
      pure $ Gen.CastNoReply state { egestLocations = newEgestLocations }

-- Called by IngestAggregator to indicate stream on this node
announceStreamIsAvailable :: StreamId -> Effect Unit
announceStreamIsAvailable streamId =
  Gen.doCast serverName
    \state@{ streamAggregatorLocations, thisNode, transPoPApi:{announceStreamIsAvailable: transPoP_announceStreamIsAvailable} } -> do
      logIfNew streamId streamAggregatorLocations "New local stream is available" {streamId}
      _ <- sendToIntraSerfNetwork state "streamAvailable" (IMStreamState StreamAvailable streamId thisNode)
      _ <- transPoP_announceStreamIsAvailable streamId thisNode

      Gen.CastNoReply <$> insertStreamAggregator streamId thisNode state

insertStreamAggregator :: StreamId -> ServerAddress -> State -> Effect State
insertStreamAggregator streamId server state@{ streamAggregatorLocations } =
  do
    maybeServerLocation <- PoPDefinition.whereIsServer server

    case maybeServerLocation of
      Nothing ->
        do
          _ <- logWarning "Received stream aggregator from unresolveable location." { server }
          pure state

      Just serverLocation ->
        let
          aggregatorInfo = LocatedServer server serverLocation
        in
          do
            newStreamAggregatorLocations <- EMap.insert' streamId aggregatorInfo streamAggregatorLocations
            pure $ state { streamAggregatorLocations = newStreamAggregatorLocations }

-- Called by IngestAggregator to indicate stream stopped on this node
announceStreamStopped :: StreamId -> Effect Unit
announceStreamStopped streamId =
  Gen.doCast serverName
    \state@{ streamAggregatorLocations, thisNode, transPoPApi:{announceStreamStopped: transPoP_announceStreamStopped} } -> do
      _ <- logInfo "Local stream stopped" {streamId}
      _ <- sendToIntraSerfNetwork state "streamStopped" (IMStreamState StreamStopped streamId thisNode)
      _ <- transPoP_announceStreamStopped streamId thisNode

      let
        newStreamAggregatorLocations = EMap.delete streamId streamAggregatorLocations
      pure $ Gen.CastNoReply state { streamAggregatorLocations = newStreamAggregatorLocations }

-- Called by TransPoP to indicate stream that is present on a node in another PoP
announceRemoteStreamIsAvailable :: StreamId -> ServerAddress -> Effect Unit
announceRemoteStreamIsAvailable streamId addr =
  Gen.doCast serverName
    \state@{ streamAggregatorLocations } -> do
      logIfNew streamId streamAggregatorLocations "New remote stream is avaiable" {streamId}
      _ <- sendToIntraSerfNetwork state "streamAvailable" (IMStreamState StreamAvailable streamId addr)

      Gen.CastNoReply <$> insertStreamAggregator streamId addr state

-- Called by TransPoP to indicate stream that has stopped on a node in another PoP
announceRemoteStreamStopped :: StreamId -> ServerAddress -> Effect Unit
announceRemoteStreamStopped streamId addr =
  Gen.doCast serverName
    \state@{ streamAggregatorLocations } -> do
      _ <- logInfo "Remote stream has stopped" {streamId}
      _ <- sendToIntraSerfNetwork state "streamStopped" (IMStreamState StreamStopped streamId addr)
      let
        newStreamAggregatorLocations = EMap.delete streamId streamAggregatorLocations
      pure $ Gen.CastNoReply state { streamAggregatorLocations = newStreamAggregatorLocations }

-- Called by TransPoP to indicate that it is acting as this PoP's leader
announceTransPoPLeader :: Effect Unit
announceTransPoPLeader =
  Gen.doCast serverName
    \state@{ thisNode, transPoPApi:{handleRemoteLeaderAnnouncement: transPoP_announceTransPoPLeader} } -> do
      _ <- transPoP_announceTransPoPLeader thisNode
      _ <- sendToIntraSerfNetwork state "transPoPLeader" (IMTransPoPLeader thisNode)
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
  thisNode <- PoPDefinition.thisNode
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

  let
    members = membersResp
              # hush
              # fromMaybe nil
              <#> (\member@{name} -> Tuple (ServerAddress name) (Tuple member (wrap 100.0)))
              # fromFoldable
  pure
    { config
    , transPoPApi
    , serfRpcAddress
    , egestLocations: MultiMap.empty
    , streamRelayLocations: EMap.empty
    , streamAggregatorLocations: EMap.empty
    , thisNode: thisNode
    , currentTransPoPLeader: Nothing
    , members
    , expireThreshold: wrap expireThresholdMs
    , streamStateClocks: EMap.empty
    , egestStateClocks: EMap.empty
    }

shouldProcessSerfMessage :: StreamId -> Int -> EMap StreamId Int -> Boolean
shouldProcessSerfMessage streamId ltime streamStateClocks =
  case EMap.lookup streamId streamStateClocks of
    Just lastLTime
      | lastLTime > ltime -> false
    _ ->
      true

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
handleIntraPoPSerfMsg imsg state@{transPoPApi: {handleRemoteLeaderAnnouncement}} =
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
       IMStreamState stateChange streamId server ->

         case shouldProcessSerfMessage streamId ltime state.streamStateClocks of
           false -> do
             _ <- logInfo "Dropping out-of-order serf message" {streamId, stateChange}
             pure $ state

           true -> do
             newIMStreamStateClocks <- EMap.insert' streamId ltime state.streamStateClocks
             handleStreamStateChange stateChange streamId server intraMessage (state {streamStateClocks = newIMStreamStateClocks})

       IMEgestState stateChange streamId server ->
         case shouldProcessSerfMessage streamId ltime state.egestStateClocks of
           false -> do
             _ <- logInfo "Dropping out-of-order serf message" {streamId, stateChange}
             pure $ state

           true -> do
             newEgestStateClocks <- EMap.insert' streamId ltime state.egestStateClocks
             handleEgestStateChange stateChange streamId server (state {egestStateClocks = newEgestStateClocks})

       IMServerLoad server load -> do
         let
           newMembers = alter (map (\(Tuple member oldLoad) -> Tuple member load)) server state.members
         pure state{members = newMembers}

       IMTransPoPLeader server
         | server == state.thisNode -> pure state { currentTransPoPLeader = Just server }
         | otherwise -> do
           _ <- handleRemoteLeaderAnnouncement server
           pure state { currentTransPoPLeader = Just server }

handleStreamStateChange :: StreamState -> StreamId -> ServerAddress -> IntraMessage -> State -> Effect State
handleStreamStateChange stateChange streamId server intraMessage
  state@{transPoPApi: {announceStreamIsAvailable: transPoP_announceStreamIsAvailable
                      , announceStreamStopped: transPoP_announceStreamStopped}} =
  case stateChange of
    StreamAvailable
      | server == state.thisNode -> do
        -- streamAvailable on this node - we can just ignore, since appropriate action taken in announceStreamIsAvailable
        pure state
      | otherwise -> do
        -- streamAvailable on some other node in this PoP - we need to tell Trans-PoP
        --_ <- logInfo "StreamAvailable on remote node" { streamId: streamId, remoteNode: server }
        _ <- transPoP_announceStreamIsAvailable streamId server

        _ <- logIfNew streamId state.streamAggregatorLocations "StreamAvailable on remote node" { streamId: streamId, remoteNode: server }

        insertStreamAggregator streamId server state

    StreamStopped
      | server == state.thisNode -> do
        -- streamStopped on this node - we can just ignore, since appropriate action taken in announceStreamStopped
        pure state
      | otherwise -> do
        -- streamStopped on some other node in this PoP - we need to tell Trans-PoP
        _ <- transPoP_announceStreamStopped streamId server

        _ <- logInfo "StreamStopped on remote node" { streamId: streamId, remoteNode: server }
        let
          newStreamAggregatorLocations = EMap.delete streamId state.streamAggregatorLocations
        pure $ state { streamAggregatorLocations = newStreamAggregatorLocations }

handleEgestStateChange :: EgestState -> StreamId -> ServerAddress -> State -> Effect State
handleEgestStateChange stateChange streamId server state =
  case stateChange of
    EgestAvailable
      | server == state.thisNode -> do
        -- egestAvailable on this node - we can just ignore, since appropriate action taken in announceEgestIsActive
        pure state
      | otherwise -> do
        -- egestAvailable on some other node in this PoP
        _ <- logInfo "EgestAvailable on remote node" { streamId: streamId, remoteNode: server }
        newEgestLocations <- MultiMap.insert' streamId server  state.egestLocations
        pure $ state { egestLocations = newEgestLocations }

    EgestStopped
      | server == state.thisNode -> do
        -- egestStopped on this node - we can just ignore, since appropriate action taken in announceEgestStopped
        pure state
      | otherwise -> do
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
  let
    newMembers = foldl (\acc member@{ name } -> Map.insert (ServerAddress name) (Tuple member (wrap 100.0)) acc) state.members members
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
        newStreamAggregatorLocations = EMap.garbageCollect threshold streamAggregatorLocations
        newStreamRelayLocations = EMap.garbageCollect threshold streamRelayLocations
        newEgestLocations = MultiMap.garbageCollect threshold egestLocations
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

maybeLogError :: forall a b c d e. Union b (error :: e) c => Nub c d => String -> Either e a -> Record b  -> Effect Unit
maybeLogError _ (Right _) _ = pure unit
maybeLogError msg (Left err) metadata = do
  _ <- logInfo msg (Record.merge metadata {error: err})
  pure unit

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
