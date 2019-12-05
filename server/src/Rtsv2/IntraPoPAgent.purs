module Rtsv2.IntraPoPAgent
  ( startLink
  , Config
  , isStreamAvailable
  , isIngestActive
  , announceStreamIsAvailable
  , announceRemoteStreamIsAvailable
  , announceTransPoPLeader
  , whereIsIngestAggregator
  , whereIsIngestRelay
  , currentTransPoPLeader
  , bus
  ) where

import Prelude

import Bus as Bus
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process (spawnLink)
import Foreign (Foreign)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub)
import Record as Record
import Rtsv2.Env as Env
import Rtsv2.IntraPoPSerf (IpAndPort, IntraMessage)
import Rtsv2.IntraPoPSerf as Serf
import Rtsv2.PoPDefinition (ServerAddress)
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.Stream (StreamId(..), StreamVariantId(..))

type State
  = { config :: Config
    , serfRpcAddress :: IpAndPort
    , currentTransPoPLeader :: Maybe ServerAddress
    , streamRelayLocations :: Map StreamId String
    , streamAggregatorLocations :: Map StreamId String
    , thisNode :: ServerAddress
    , members :: Map String Serf.SerfMember
    }

type Config
  = { bindPort :: Int
    , rpcPort :: Int
    }

data Msg
  = JoinAll
  | SerfMessage IntraMessage

bus :: Bus.Bus IntraMessage
bus = Bus.bus "intrapop_bus"

isStreamAvailable :: StreamId -> Effect Boolean
isStreamAvailable streamId =
  Gen.doCall serverName \state@{ streamAggregatorLocations } ->
    do
      _ <- logInfo "Request for stream" {streamId : streamId,
                                         locations: streamAggregatorLocations}
      pure $ CallReply (Map.member streamId streamAggregatorLocations) state

isIngestActive :: StreamVariantId -> Effect Boolean
isIngestActive (StreamVariantId s v) = Gen.call serverName \state -> CallReply false state

whereIsIngestRelay :: StreamVariantId -> Effect (Maybe String)
whereIsIngestRelay (StreamVariantId s _) =
  Gen.call serverName \state ->
    CallReply (Map.lookup (StreamId s) state.streamRelayLocations) state

whereIsIngestAggregator :: StreamVariantId -> Effect (Maybe String)
whereIsIngestAggregator (StreamVariantId s _) =
  Gen.call serverName \state ->
    CallReply (Map.lookup (StreamId s) state.streamAggregatorLocations) state

currentTransPoPLeader :: Effect (Maybe ServerAddress)
currentTransPoPLeader =
  Gen.call serverName (\state@{currentTransPoPLeader : value} ->
                        CallReply value state)

announceStreamIsAvailable :: StreamId -> Effect Unit
announceStreamIsAvailable streamId =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations, thisNode } -> do
        _ <- logInfo "Local stream available" {streamId : streamId}
        _ <- raiseLocal state "streamAvailable" (Serf.StreamAvailable streamId thisNode) true
        pure $ Gen.CastNoReply state { streamAggregatorLocations = (Map.insert streamId thisNode streamAggregatorLocations) }

announceRemoteStreamIsAvailable :: StreamId -> ServerAddress -> Effect Unit
announceRemoteStreamIsAvailable streamId addr =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations } -> do
        _ <- logInfo "Remote stream available" {streamId : streamId,
                                                addr: addr}
        _ <- raiseLocal state "streamAvailable" (Serf.StreamAvailable streamId addr) true
        pure $ Gen.CastNoReply state { streamAggregatorLocations = (Map.insert streamId addr streamAggregatorLocations) }

announceTransPoPLeader :: Effect Unit
announceTransPoPLeader =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations, thisNode } -> do
        _ <- raiseLocal state "transPoPLeader" (Serf.TransPoPLeader thisNode) false
        pure $ Gen.CastNoReply state

raiseLocal :: State -> String -> IntraMessage -> Boolean -> Effect Unit
raiseLocal state name msg coalesce = do
  _ <- Serf.event state.serfRpcAddress name msg coalesce
  _ <- Bus.raise bus msg
  pure unit

serverName :: ServerName State Msg
serverName = Local "intraPopAgent"

startLink :: Config -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

init :: Config -> Effect State
init config = do
  _ <- Gen.registerExternalMapping serverName (\m -> SerfMessage <$> (Serf.messageMapper m))
  _ <- Timer.sendAfter serverName 0 JoinAll
  rpcBindIp <- Env.privateInterfaceIp
  thisNode <- PoPDefinition.thisNode
  let
    serfRpcAddress =
      { ip: show rpcBindIp
      , port: config.rpcPort
      }
  _ <- Serf.stream serfRpcAddress
  _ <-
    logInfo "Intra-PoP Agent Starting"
      { config: config
      }
  pure
    { config
    , serfRpcAddress
    , streamRelayLocations: Map.empty
    , streamAggregatorLocations: Map.empty
    , thisNode: thisNode
    , currentTransPoPLeader: Nothing
    , members: Map.empty
    }

handleInfo :: Msg -> State -> Effect State
handleInfo msg state = case msg of
  SerfMessage intraMessage -> case intraMessage of

    Serf.Ignore ->
      pure state

    Serf.StreamAvailable streamId server
      | server == state.thisNode -> do
        _ <- logInfo "serf notification about stream available on this node; ignoring" {streamId: streamId,
                                                                                        server: server}
        pure state
      | otherwise -> do
        _ <- Bus.raise bus intraMessage
        _ <- logInfo "StreamAvailable on remote node" { streamId: streamId
                                                      , remoteNode: server
                                                      }
        pure $ state { streamAggregatorLocations = (Map.insert streamId server state.streamAggregatorLocations) }

    Serf.TransPoPLeader server
      | server == state.thisNode -> pure state{currentTransPoPLeader = Just server}
      | otherwise -> do
        _ <- Bus.raise bus intraMessage
        pure state{currentTransPoPLeader = Just server}

    Serf.MembersAlive members -> do
      _ <- logInfo "Members Alive" {members: members}
      let
        newMembers = foldl (\acc member@{name} -> Map.insert name member acc) state.members members
      pure state {members = newMembers}

    Serf.MembersLeft members -> do
      _ <- logInfo "Members Left" {members: members}
      let
        newMembers = foldl (\acc {name} -> Map.delete name acc) state.members members
      pure state {members = newMembers}


  JoinAll -> do
    _ <- joinAllSerf state
    pure state

joinAllSerf :: State -> Effect Unit
joinAllSerf { config, serfRpcAddress } =
  let
    serverAddressToSerfAddress s = {ip: s,
                                    port: config.bindPort}
  in
  do
    -- TODO - could spawn a process per seed and issue the joins in parallel
    seeds <- PoPDefinition.getSeedsForThisPoP :: Effect (List String)
    process <-
      spawnLink
        ( \_ -> do
            result <- Serf.join serfRpcAddress (serverAddressToSerfAddress <$> seeds) true
            _ <- logInfo "Serf said " { result: result }
            pure unit
        )
    pure unit

logInfo :: forall a b. Nub ( domain :: List Atom | a ) b => String -> Record a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.IntraPoP)) : nil) } { misc: metaData })
