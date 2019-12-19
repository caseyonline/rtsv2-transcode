module Rtsv2.IntraPoPAgent
  ( startLink
  , isStreamIngestAvailable
  , isIngestActive
  , announceEdgeIsAvailable
  , announceEdgeStopped
  , announceStreamIsAvailable
  , announceStreamStopped
  , announceRemoteStreamIsAvailable
  , announceRemoteStreamStopped
  , announceTransPoPLeader
  , whereIsIngestAggregator
  , whereIsStreamRelay
  , whereIsEdge
  , currentTransPoPLeader
  , health
  , IntraMessage(..)
  , StreamState(..)
  , EdgeState(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Traversable (traverse_)
import Effect (Effect)
import Ephemeral.Map (EMap)
import Ephemeral.Map as EMap
import Ephemeral.MultiMap (EMultiMap)
import Ephemeral.MultiMap as MultiMap
import Erl.Atom (atom)
import Erl.Data.List (List, length, nil, singleton, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process (Process, spawnLink)
import Erl.Utils (Milliseconds)
import Erl.Utils as Erl
import Foreign (Foreign)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub, class Union)
import Record as Record
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Health (Health, percentageToHealth)
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (IpAndPort)
import Serf as Serf
import Shared.Agent as Agent
import Shared.Stream (StreamId(..), StreamVariantId(..))
import Shared.Types (ServerAddress(..))

type State
  = { transPoPApi :: Config.TransPoPAgentApi
    , config :: Config.IntraPoPAgentConfig
    , serfRpcAddress :: IpAndPort
    , currentTransPoPLeader :: Maybe ServerAddress
    , edgeLocations :: EMultiMap StreamId ServerAddress
    , streamRelayLocations :: EMap StreamId ServerAddress
    , streamAggregatorLocations :: EMap StreamId ServerAddress
    , thisNode :: ServerAddress
    , members :: Map ServerAddress Serf.SerfMember
    , expireThreshold :: Milliseconds
    , streamStateClocks :: EMap StreamId Int
    , edgeStateClocks :: EMap StreamId Int
    }

data StreamState = StreamAvailable
                 | StreamStopped

data EdgeState = EdgeAvailable
               | EdgeStopped

data IntraMessage = StreamState StreamState StreamId ServerAddress
                  | EdgeState EdgeState StreamId ServerAddress
                  | TransPoPLeader ServerAddress

data Msg
  = JoinAll
  | GarbageCollect
  | IntraPoPSerfMsg (Serf.SerfMessage IntraMessage)

serverName :: ServerName State Msg
serverName = Local $ show Agent.IntraPoP

health :: Effect Health
health =
  Gen.doCall serverName \state@{ members } -> do
    allOtherServers <- PoPDefinition.getOtherServersForThisPoP
    let
      currentHealth = percentageToHealth $ (Map.size members * 100) / (length allOtherServers) * 100
    pure $ CallReply currentHealth state

isStreamIngestAvailable :: StreamId -> Effect Boolean
isStreamIngestAvailable streamId =
  Gen.call serverName \state@{ streamAggregatorLocations } ->
    CallReply (EMap.member streamId streamAggregatorLocations) state

isIngestActive :: StreamVariantId -> Effect Boolean
isIngestActive (StreamVariantId s v) = Gen.call serverName \state -> CallReply false state

whereIsIngestAggregator :: StreamVariantId -> Effect (Maybe ServerAddress)
whereIsIngestAggregator (StreamVariantId streamId _) =
  Gen.call serverName \state ->
    CallReply (EMap.lookup (StreamId streamId) state.streamAggregatorLocations) state

whereIsStreamRelay :: StreamId -> Effect (Maybe ServerAddress)
whereIsStreamRelay streamId =
  Gen.call serverName \state ->
    CallReply (EMap.lookup streamId state.streamRelayLocations) state

whereIsEdge :: StreamId -> Effect (List ServerAddress)
whereIsEdge streamId =
  Gen.call serverName \state ->
    CallReply (MultiMap.lookup streamId state.edgeLocations) state

currentTransPoPLeader :: Effect (Maybe ServerAddress)
currentTransPoPLeader =
  Gen.call serverName
    ( \state@{ currentTransPoPLeader: value } ->
        CallReply value state
    )

-- Called by EdgeAgent to indicate edge on this node
announceEdgeIsAvailable :: StreamId -> Effect Unit
announceEdgeIsAvailable streamId =
  Gen.doCast serverName
    $ \state@{ edgeLocations, thisNode } -> do
        _ <- logInfo "Local edge available" { streamId: streamId }
        _ <- sendToIntraSerfNetwork state "edgeAvailable" (EdgeState EdgeAvailable streamId thisNode)
        newEdgeLocations <- MultiMap.insert' streamId thisNode  state.edgeLocations
        pure $ Gen.CastNoReply state { edgeLocations = newEdgeLocations }

-- Called by EdgeAgent to indicate edge on this node has stopped
announceEdgeStopped :: StreamId -> Effect Unit
announceEdgeStopped streamId =
  Gen.doCast serverName
    $ \state@{ edgeLocations, thisNode } -> do
        _ <- logInfo "Local edge stopped" {streamId}
        _ <- sendToIntraSerfNetwork state "edgeStopped" (EdgeState EdgeStopped streamId thisNode)
        let
          newEdgeLocations = MultiMap.delete streamId thisNode edgeLocations
        pure $ Gen.CastNoReply state { edgeLocations = newEdgeLocations }

-- Called by IngestAggregator to indicate stream on this node
announceStreamIsAvailable :: StreamId -> Effect Unit
announceStreamIsAvailable streamId =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations, thisNode, transPoPApi:{announceStreamIsAvailable: transPoP_announceStreamIsAvailable} } -> do
        logIfNew streamId streamAggregatorLocations "New local stream is available" {streamId}
        _ <- sendToIntraSerfNetwork state "streamAvailable" (StreamState StreamAvailable streamId thisNode)
        _ <- transPoP_announceStreamIsAvailable streamId thisNode

        newStreamAggregatorLocations <- EMap.insert' streamId thisNode streamAggregatorLocations
        pure $ Gen.CastNoReply state { streamAggregatorLocations = newStreamAggregatorLocations }

-- Called by IngestAggregator to indicate stream stopped on this node
announceStreamStopped :: StreamId -> Effect Unit
announceStreamStopped streamId =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations, thisNode, transPoPApi:{announceStreamStopped: transPoP_announceStreamStopped} } -> do
        _ <- logInfo "Local stream stopped" {streamId}
        _ <- sendToIntraSerfNetwork state "streamStopped" (StreamState StreamStopped streamId thisNode)
        _ <- transPoP_announceStreamStopped streamId thisNode

        let
          newStreamAggregatorLocations = EMap.delete streamId streamAggregatorLocations
        pure $ Gen.CastNoReply state { streamAggregatorLocations = newStreamAggregatorLocations }

-- Called by TransPoP to indicate stream that is present on a node in another PoP
announceRemoteStreamIsAvailable :: StreamId -> ServerAddress -> Effect Unit
announceRemoteStreamIsAvailable streamId addr =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations } -> do
        logIfNew streamId streamAggregatorLocations "New remote stream is avaiable" {streamId}
        _ <- sendToIntraSerfNetwork state "streamAvailable" (StreamState StreamAvailable streamId addr)
        newStreamAggregatorLocations <- EMap.insert' streamId addr streamAggregatorLocations
        pure $ Gen.CastNoReply state { streamAggregatorLocations = newStreamAggregatorLocations }

-- Called by TransPoP to indicate stream that has stopped on a node in another PoP
announceRemoteStreamStopped :: StreamId -> ServerAddress -> Effect Unit
announceRemoteStreamStopped streamId addr =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations } -> do
        _ <- logInfo "Remote stream has stopped" {streamId}
        _ <- sendToIntraSerfNetwork state "streamStopped" (StreamState StreamStopped streamId addr)
        let
          newStreamAggregatorLocations = EMap.delete streamId streamAggregatorLocations
        pure $ Gen.CastNoReply state { streamAggregatorLocations = newStreamAggregatorLocations }

logIfNew :: forall a v. StreamId -> EMap StreamId v -> String -> Record a -> Effect Unit
logIfNew streamId m str metadata =
  if EMap.member streamId m
  then pure unit
  else void $ logInfo str metadata

announceTransPoPLeader :: Effect Unit
announceTransPoPLeader =
  Gen.doCast serverName
    $ \state@{ thisNode, transPoPApi:{announceTransPoPLeader: transPoP_announceTransPoPLeader} } -> do
      _ <- transPoP_announceTransPoPLeader thisNode
      _ <- sendToIntraSerfNetwork state "transPoPLeader" (TransPoPLeader thisNode)
      pure $ Gen.CastNoReply state

sendToIntraSerfNetwork :: State -> String -> IntraMessage -> Effect Unit
sendToIntraSerfNetwork state name msg = do
  result <- Serf.event state.serfRpcAddress name msg false
  _ <- maybeLogError "Intra-PoP serf event failed" result {}
  pure unit

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
  streamResp <- Serf.stream serfRpcAddress
  _ <- case streamResp of
         Left error -> do
           _ <- logInfo "Could not connect to IntraPoP Serf Agent" { error: error }
           _ <- Erl.sleep (wrap 100) -- Just so we don't spin like crazy...
           unsafeCrashWith ("could_not_connect_stream")
         Right r ->
           pure r

  pure
    { config
    , transPoPApi
    , serfRpcAddress
    , edgeLocations: MultiMap.empty
    , streamRelayLocations: EMap.empty
    , streamAggregatorLocations: EMap.empty
    , thisNode: thisNode
    , currentTransPoPLeader: Nothing
    , members: Map.empty
    , expireThreshold: wrap expireThresholdMs
    , streamStateClocks: EMap.empty
    , edgeStateClocks: EMap.empty
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
handleIntraPoPSerfMsg imsg state@{transPoPApi: {announceTransPoPLeader: transPoP_announceTransPoPLeader}} =
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
       StreamState stateChange streamId server ->

         case shouldProcessSerfMessage streamId ltime state.streamStateClocks of
           false -> do
             _ <- logInfo "Dropping out-of-order serf message" {streamId, stateChange}
             pure $ state

           true -> do
             newStreamStateClocks <- EMap.insert' streamId ltime state.streamStateClocks
             handleStreamStateChange stateChange streamId server intraMessage (state {streamStateClocks = newStreamStateClocks})

       EdgeState stateChange streamId server ->
         case shouldProcessSerfMessage streamId ltime state.edgeStateClocks of
           false -> do
             _ <- logInfo "Dropping out-of-order serf message" {streamId, stateChange}
             pure $ state

           true -> do
             newEdgeStateClocks <- EMap.insert' streamId ltime state.edgeStateClocks
             handleEdgeStateChange stateChange streamId server (state {edgeStateClocks = newEdgeStateClocks})

       TransPoPLeader server
         | server == state.thisNode -> pure state { currentTransPoPLeader = Just server }
         | otherwise -> do
           _ <- transPoP_announceTransPoPLeader server
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

        logIfNew streamId state.streamAggregatorLocations "StreamAvailable on remote node" { streamId: streamId, remoteNode: server }
        newStreamAggregatorLocations <- EMap.insert' streamId server state.streamAggregatorLocations
        pure $ state { streamAggregatorLocations = newStreamAggregatorLocations }

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

handleEdgeStateChange :: EdgeState -> StreamId -> ServerAddress -> State -> Effect State
handleEdgeStateChange stateChange streamId server state =
  case stateChange of
    EdgeAvailable
      | server == state.thisNode -> do
        -- edgeAvailable on this node - we can just ignore, since appropriate action taken in announceEdgeIsActive
        pure state
      | otherwise -> do
        -- edgeAvailable on some other node in this PoP
        _ <- logInfo "EdgeAvailable on remote node" { streamId: streamId, remoteNode: server }
        newEdgeLocations <- MultiMap.insert' streamId server  state.edgeLocations
        pure $ state { edgeLocations = newEdgeLocations }

    EdgeStopped
      | server == state.thisNode -> do
        -- edgeStopped on this node - we can just ignore, since appropriate action taken in announceEdgeStopped
        pure state
      | otherwise -> do
        _ <- logInfo "EdgeStopped on remote node" { streamId: streamId, remoteNode: server }
        let
          newEdgeLocations = MultiMap.delete streamId server state.edgeLocations
        pure $ state { edgeLocations = newEdgeLocations }

membersAlive :: (List Serf.SerfMember) -> State -> Effect State
membersAlive members state = do
  _ <- logInfo "Members Alive" { members: _.name <$> members }
  let
    newMembers = foldl (\acc member@{ name } -> Map.insert (ServerAddress name) member acc) state.members members
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
                     , edgeLocations
                     } =
  do
    now <- Erl.systemTimeMs
    let threshold = now - expireThreshold
        newStreamAggregatorLocations = EMap.garbageCollect threshold streamAggregatorLocations
        newStreamRelayLocations = EMap.garbageCollect threshold streamRelayLocations
        newEdgeLocations = MultiMap.garbageCollect threshold edgeLocations
    pure state{ streamAggregatorLocations = newStreamAggregatorLocations
              , streamRelayLocations = newStreamRelayLocations
              , edgeLocations = newEdgeLocations
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

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.IntraPoP)) : nil) } { misc: metaData })
