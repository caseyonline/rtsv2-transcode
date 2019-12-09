module Rtsv2.IntraPoPAgent
  ( startLink
  , isStreamIngestAvailable
  , isIngestActive
  , announceEdgeIsActive
  , announceStreamIsAvailable
  , announceRemoteStreamIsAvailable
  , announceTransPoPLeader
  , whereIsIngestAggregator
  , whereIsStreamRelay
  , currentTransPoPLeader
  , health
  , bus
  , IntraMessage(..)
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse_)
import Effect (Effect)
import EphemeralMap (EMap)
import EphemeralMap as EMap
import Erl.Atom (Atom, atom)
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
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub, class Union)
import Record as Record
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Health (Health, percentageToHealth)
import Rtsv2.PoPDefinition (ServerAddress)
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (IpAndPort)
import Serf as Serf
import Shared.Agent as Agent
import Shared.Stream (StreamId(..), StreamVariantId(..))

type State
  = { config :: Config.IntraPoPAgentConfig
    , serfRpcAddress :: IpAndPort
    , currentTransPoPLeader :: Maybe ServerAddress
    , edgeLocations :: Map StreamId (Set ServerAddress) -- TODO - ServerAddresses need to be ephemeral
    , streamRelayLocations :: EMap StreamId ServerAddress
    , streamAggregatorLocations :: EMap StreamId ServerAddress
    , thisNode :: ServerAddress
    , members :: Map ServerAddress Serf.SerfMember
    , expireThreshold :: Milliseconds
    }

data IntraMessage = StreamAvailable StreamId ServerAddress
                  | EdgeAvailable StreamId ServerAddress
                  | TransPoPLeader ServerAddress

data Msg
  = JoinAll
  | GarbageCollect
  | IntraPoPSerfMsg (Serf.SerfMessage IntraMessage)

bus :: Bus.Bus IntraMessage
bus = Bus.bus "intrapop_bus"

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

whereIsIngestAggregator :: StreamVariantId -> Effect (Maybe String)
whereIsIngestAggregator (StreamVariantId streamId _) =
  Gen.call serverName \state ->
    CallReply (EMap.lookup (StreamId streamId) state.streamAggregatorLocations) state

whereIsStreamRelay :: StreamId -> Effect (Maybe String)
whereIsStreamRelay (StreamId streamId) =
  Gen.call serverName \state ->
    CallReply (EMap.lookup (StreamId streamId) state.streamRelayLocations) state

currentTransPoPLeader :: Effect (Maybe ServerAddress)
currentTransPoPLeader =
  Gen.call serverName
    ( \state@{ currentTransPoPLeader: value } ->
        CallReply value state
    )

-- Called by EdgeAgent to indicate edge on this node
announceEdgeIsActive :: StreamId -> Effect Unit
announceEdgeIsActive streamId =
  Gen.doCast serverName
    $ \state@{ edgeLocations, thisNode } -> do
        --_ <- logInfo "Local edge available" { streamId: streamId }
        _ <- raiseLocal state "edgeAvailable" (EdgeAvailable streamId thisNode)
        let
          currentLocations = fromMaybe Set.empty $ Map.lookup streamId state.edgeLocations
          newLocations = Set.insert thisNode currentLocations
          newEdgeLocations = Map.insert streamId newLocations state.edgeLocations
        pure $ Gen.CastNoReply state { edgeLocations = newEdgeLocations }

-- Called by IngestAggregator to indicate stream on this node
announceStreamIsAvailable :: StreamId -> Effect Unit
announceStreamIsAvailable streamId =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations, thisNode } -> do
        --_ <- logInfo "Local stream available" { streamId: streamId }
        _ <- raiseLocal state "streamAvailable" (StreamAvailable streamId thisNode)
        newStreamAggregatorLocations <- EMap.insert' streamId thisNode streamAggregatorLocations
        pure $ Gen.CastNoReply state { streamAggregatorLocations = newStreamAggregatorLocations }

-- Called by TransPoP to indicate stream that is present on a node in another PoP
announceRemoteStreamIsAvailable :: StreamId -> ServerAddress -> Effect Unit
announceRemoteStreamIsAvailable streamId addr =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations } -> do
        _ <- logInfo "Remote stream available" { streamId: streamId
                                               , addr: addr }
        result <- Serf.event state.serfRpcAddress "streamAvailable" (StreamAvailable streamId addr) false
        _ <- maybeLogError "Intra-PoP serf event failed" result {}
        -- _ <- raiseLocal state "streamAvailable" (StreamAvailable streamId addr)
        newStreamAggregatorLocations <- EMap.insert' streamId addr streamAggregatorLocations
        pure $ Gen.CastNoReply state { streamAggregatorLocations = newStreamAggregatorLocations }

announceTransPoPLeader :: Effect Unit
announceTransPoPLeader =
  Gen.doCast serverName
    $ \state@{ thisNode } -> do
        _ <- raiseLocal state "transPoPLeader" (TransPoPLeader thisNode)
        pure $ Gen.CastNoReply state

raiseLocal :: State -> String -> IntraMessage -> Effect Unit
raiseLocal state name msg = do
  result <- Serf.event state.serfRpcAddress name msg false
  _ <- maybeLogError "Intra-PoP serf event failed" result {}
  _ <- Bus.raise bus msg
  pure unit

startLink :: Config.IntraPoPAgentConfig -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

init :: Config.IntraPoPAgentConfig -> Effect State
init config@{rejoinEveryMs
             , expireThresholdMs
             , expireEveryMs}
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
    , serfRpcAddress
    , edgeLocations: Map.empty
    , streamRelayLocations: EMap.empty
    , streamAggregatorLocations: EMap.empty
    , thisNode: thisNode
    , currentTransPoPLeader: Nothing
    , members: Map.empty
    , expireThreshold: wrap expireThresholdMs
    }

handleInfo :: Msg -> State -> Effect State
handleInfo msg state = case msg of
  JoinAll -> do
    _ <- joinAllSerf state
    pure state
  GarbageCollect -> garbageCollect state
  IntraPoPSerfMsg imsg ->
   case imsg of
    Serf.MemberAlive members -> membersAlive members state
    Serf.MemberLeaving -> pure state
    Serf.MemberLeft members -> membersLeft members state
    Serf.MemberFailed -> pure state
    Serf.StreamFailed -> do
      _ <- logInfo "Lost connection to IntraPoP Serf Agent" {}
      _ <- Erl.sleep (wrap 100) -- Just so we don't spin like crazy...
      unsafeCrashWith ("lost_serf_connection")
    Serf.UserEvent name ltime coalesce intraMessage ->
     case intraMessage of
      StreamAvailable streamId server
        | server == state.thisNode -> do
          -- streamAvailable on this node - we can just ignore, since appropriate action taken in announceStreamIsAvailable
          pure state
        | otherwise -> do
          -- streamAvailable on some other node in this PoP - we need to tell Trans-PoP
          _ <- Bus.raise bus intraMessage
          -- _ <- logInfo "StreamAvailable on remote node" { streamId: streamId, remoteNode: server }
          newStreamAggregatorLocations <- EMap.insert' streamId server state.streamAggregatorLocations
          pure $ state { streamAggregatorLocations = newStreamAggregatorLocations }
      EdgeAvailable streamId server
        | server == state.thisNode -> do
          -- edgeAvailable on this node - we can just ignore, since appropriate action taken in announceEdgeIsActive
          pure state
        | otherwise -> do
          -- edgeAvailable on some other node in this PoP
          -- _ <- logInfo "EdgeAvailable on remote node" { streamId: streamId, remoteNode: server }
          let
            currentLocations = fromMaybe Set.empty $ Map.lookup streamId state.edgeLocations
            newLocations = Set.insert server currentLocations
            newEdgeLocations = Map.insert streamId newLocations state.edgeLocations
          pure $ state { edgeLocations = newEdgeLocations }
      TransPoPLeader server
        | server == state.thisNode -> pure state { currentTransPoPLeader = Just server }
        | otherwise -> do
          _ <- Bus.raise bus intraMessage
          pure state { currentTransPoPLeader = Just server }

membersAlive :: (List Serf.SerfMember) -> State -> Effect State
membersAlive members state = do
  _ <- logInfo "Members Alive" { members: _.name <$> members }
  let
    newMembers = foldl (\acc member@{ name } -> Map.insert name member acc) state.members members
  pure state { members = newMembers }

membersLeft :: (List Serf.SerfMember) -> State -> Effect State
membersLeft members state = do
  _ <- logInfo "Members Left" { members: _.name <$> members }
  let
    newMembers = foldl (\acc { name } -> Map.delete name acc) state.members members
  pure state { members = newMembers }

garbageCollect :: State -> Effect State
garbageCollect state@{expireThreshold,
                      streamAggregatorLocations} =
  do
    newStreamAggregatorLocations <- EMap.garbageCollect' expireThreshold streamAggregatorLocations
    pure state{streamAggregatorLocations = newStreamAggregatorLocations}

joinAllSerf :: State -> Effect Unit
joinAllSerf { config, serfRpcAddress, members } =
  let
    serverAddressToSerfAddress s =
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
