module Rtsv2.IntraPoPAgent
  ( startLink
  , Config
  , isStreamAvailable
  , isIngestActive
  , streamIsAvailable
  , whereIsIngestAggregator
  , whereIsIngestRelay
  , bus
  ) where

import Prelude

import Bus as Bus
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Process (SpawnedProcessState, spawnLink)
import Foreign (Foreign)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub)
import Record as Record
import Rtsv2.Env as Env
import Rtsv2.PoPDefinition (ServerAddress, thisNode)
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Serf (Ip, IpAndPort, StateMessage)
import Rtsv2.Serf as Serf
import Shared.Agent as Agent
import Shared.Stream (StreamId(..), StreamVariantId(..))

type State
  = { config :: Config
    , serfRpcAddress :: IpAndPort
    , streamRelayLocations :: Map StreamId String
    , streamAggregatorLocations :: Map StreamId String
    }

type Config
  = { bindPort :: Int
    , rpcPort :: Int
    }

data Msg
  = JoinAll
  | SerfMessage (Serf.Message StateMessage)

bus :: Bus.Bus StateMessage
bus = Bus.bus "intrapop_bus"

isStreamAvailable :: StreamId -> Effect Boolean
isStreamAvailable streamId = Gen.call serverName \state@{streamAggregatorLocations} -> CallReply (Map.member streamId streamAggregatorLocations) state

isIngestActive :: StreamVariantId -> Effect Boolean
isIngestActive (StreamVariantId s v) = Gen.call serverName \state -> CallReply false state

whereIsIngestRelay :: StreamVariantId -> Effect (Maybe String)
whereIsIngestRelay (StreamVariantId s _) = Gen.call serverName \state ->
  CallReply (Map.lookup (StreamId s) state.streamRelayLocations) state

whereIsIngestAggregator :: StreamVariantId -> Effect (Maybe String)
whereIsIngestAggregator (StreamVariantId s _) = Gen.call serverName \state ->
  CallReply (Map.lookup (StreamId s) state.streamAggregatorLocations) state


streamIsAvailable :: StreamId -> Effect Unit
streamIsAvailable s =
  Gen.doCast serverName
    $ \state@{streamAggregatorLocations} -> do
      thisNode <- thisNode
      -- TODO - when we raise a serf event, we also need to raise on our local message bus
      _ <- Serf.event state.serfRpcAddress "streamAvailable" (Serf.StreamAvailable s thisNode) true
      pure $ Gen.CastNoReply state { streamAggregatorLocations = (Map.insert s thisNode streamAggregatorLocations ) }

announceTransPoPLeader :: ServerAddress -> Effect Unit
announceTransPoPLeader s =
  -- todo
  pure unit

serverName :: ServerName State Msg
serverName = Local "intraPopAgent"

startLink :: Config -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

init :: Config -> Effect State
init config = do
  _ <- Timer.sendAfter serverName 0 JoinAll
  rpcBindIp <- Env.privateInterfaceIp
  let
    serfRpcAddress =
      { ip: show rpcBindIp
      , port: config.rpcPort
      }

  _ <- Gen.registerExternalMapping serverName  (Just <<< SerfMessage <<< Serf.messageMapper)
  _ <- Serf.stream serfRpcAddress

  _ <- logInfo "Intra-PoP Agent Starting"
        { config: config
        }

  pure { config
       , serfRpcAddress
       , streamRelayLocations : Map.empty
       , streamAggregatorLocations : Map.empty
       }

handleInfo :: Msg -> State -> Effect State
handleInfo msg state =
  case msg of
    SerfMessage (Serf.UserEvent name lamportClock coalesce stateMessage) ->

      do
        _ <- Bus.raise bus stateMessage

        case stateMessage of
          Serf.StreamAvailable streamId server ->
            do
              thisNode <- thisNode
              _ <- logInfo "StreamAvailable on remote node" { streamId: streamId
                                                            , remoteNode: server}
              pure $ state { streamAggregatorLocations = (Map.insert streamId server state.streamAggregatorLocations )}

          other -> do
            _ <- logInfo "Holy cow" {serf: other}
            pure state

    SerfMessage _ ->
      pure state

    JoinAll -> do
      _ <- joinAllSerf state
      pure state

joinAllSerf :: State -> Effect Unit
joinAllSerf {config, serfRpcAddress}  =
  do
    process <- spawnLink (\_ ->
                           do
                             seeds <- PoPDefinition.getSeeds :: Effect (List String)
                             let
                               seedAddresses =
                                 map
                                 ( \s ->
                                    { ip: s
                                    , port: config.bindPort
                                    }
                                 )
                                 seeds
                             result <- Serf.join serfRpcAddress seedAddresses true
                             _ <- logInfo "Serf said " { result: result }
                             pure unit
                         )
    pure unit

logInfo :: forall a b. Nub ( domain :: List Atom | a ) b => String -> Record a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.IntraPoP)) : nil) } { misc: metaData })
