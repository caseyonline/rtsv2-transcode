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
  , health
  , bus
  , IntraMessage(..)
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, length, nil, singleton, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process (Process, spawnLink)
import Erl.Utils as Erl
import Foreign (Foreign)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub)
import Record as Record
import Rtsv2.Env as Env
import Rtsv2.Health (Health, percentageToHealth)
import Rtsv2.PoPDefinition (ServerAddress)
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (IpAndPort)
import Serf as Serf
import Shared.Agent as Agent
import Shared.Stream (StreamId(..), StreamVariantId(..))

type State
  = { config :: Config
    , serfRpcAddress :: IpAndPort
    , currentTransPoPLeader :: Maybe ServerAddress
    , streamRelayLocations :: Map StreamId String
    , streamAggregatorLocations :: Map StreamId String
    , thisNode :: ServerAddress
    , members :: Map ServerAddress Serf.SerfMember
    }


data IntraMessage = StreamAvailable StreamId ServerAddress
                  | TransPoPLeader ServerAddress


type Config
  = { bindPort :: Int
    , rpcPort :: Int
    , rejoinEveryMs :: Int
    }

data Msg
  = JoinAll
  | IntraPoPSerfMsg (Serf.SerfMessage IntraMessage)

bus :: Bus.Bus IntraMessage
bus = Bus.bus "intrapop_bus"

health :: Effect Health
health =
  Gen.doCall serverName \state@{ members } -> do
    allOtherServers <- PoPDefinition.getOtherServersForThisPoP
    let
      currentHealth = percentageToHealth $ (Map.size members * 100) / (length allOtherServers) * 100
    pure $ CallReply currentHealth state

isStreamAvailable :: StreamId -> Effect Boolean
isStreamAvailable streamId =
  Gen.call serverName \state@{ streamAggregatorLocations } ->
    CallReply (Map.member streamId streamAggregatorLocations) state

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
  Gen.call serverName
    ( \state@{ currentTransPoPLeader: value } ->
        CallReply value state
    )

announceStreamIsAvailable :: StreamId -> Effect Unit
announceStreamIsAvailable streamId =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations, thisNode } -> do
        _ <- logInfo "Local stream available" { streamId: streamId }
        _ <- raiseLocal state "streamAvailable" (StreamAvailable streamId thisNode)
        pure $ Gen.CastNoReply state { streamAggregatorLocations = (Map.insert streamId thisNode streamAggregatorLocations) }

announceRemoteStreamIsAvailable :: StreamId -> ServerAddress -> Effect Unit
announceRemoteStreamIsAvailable streamId addr =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations } -> do
        _ <- logInfo "Remote stream available" { streamId: streamId
                                               , addr: addr }
        _ <- raiseLocal state "streamAvailable" (StreamAvailable streamId addr)
        pure $ Gen.CastNoReply state { streamAggregatorLocations = (Map.insert streamId addr streamAggregatorLocations) }

announceTransPoPLeader :: Effect Unit
announceTransPoPLeader =
  Gen.doCast serverName
    $ \state@{ streamAggregatorLocations, thisNode } -> do
        _ <- raiseLocal state "transPoPLeader" (TransPoPLeader thisNode)
        pure $ Gen.CastNoReply state

raiseLocal :: State -> String -> IntraMessage -> Effect Unit
raiseLocal state name msg = do
  resp <- Serf.event state.serfRpcAddress name msg false
  _ <- Bus.raise bus msg
  pure unit

serverName :: ServerName State Msg
serverName = Local "intraPopAgent"

startLink :: Config -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

init :: Config -> Effect State
init config = do
  _ <- logInfo "Intra-PoP Agent Starting" {config: config}
  _ <- Gen.registerExternalMapping serverName (\m -> IntraPoPSerfMsg <$> (Serf.messageMapper m))
  _ <- Timer.sendAfter serverName 0 JoinAll
  _ <- Timer.sendEvery serverName config.rejoinEveryMs JoinAll
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
           _ <- Erl.sleep 100 -- Just so we don't spin like crazy...
           unsafeCrashWith ("could_not_connect_stream")
         Right r ->
           pure r

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
  JoinAll -> do
    _ <- joinAllSerf state
    pure state
  IntraPoPSerfMsg imsg ->
   case imsg of
    Serf.MemberAlive members -> membersAlive members state
    Serf.MemberLeaving -> pure state
    Serf.MemberLeft members -> membersLeft members state
    Serf.MemberFailed -> pure state
    Serf.StreamFailed -> do
      _ <- logInfo "Lost connection to IntraPoP Serf Agent" {}
      _ <- Erl.sleep 100 -- Just so we don't spin like crazy...
      unsafeCrashWith ("lost_serf_connection")
    Serf.UserEvent name ltime coalesce intraMessage ->
     case intraMessage of
      StreamAvailable streamId server
        | server == state.thisNode -> do
          _ <-
            logInfo "serf notification about stream available on this node; ignoring"
              { streamId: streamId
              , server: server
              }
          pure state
        | otherwise -> do
          _ <- Bus.raise bus intraMessage
          _ <-
            logInfo "StreamAvailable on remote node"
              { streamId: streamId
              , remoteNode: server
              }
          pure $ state { streamAggregatorLocations = (Map.insert streamId server state.streamAggregatorLocations) }
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
                  _ <- logInfo "Serf said " { result: result }
                  pure unit
              )
        traverse_ joinAsync toJoin
  where
  toMap :: forall a. List a -> Map a Unit
  toMap list = foldl (\acc item -> Map.insert item unit acc) Map.empty list

logInfo :: forall a b. Nub ( domain :: List Atom | a ) b => String -> Record a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.IntraPoP)) : nil) } { misc: metaData })
