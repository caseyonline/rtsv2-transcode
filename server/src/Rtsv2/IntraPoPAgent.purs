module Rtsv2.IntraPoPAgent
  ( startLink
  , Config
  , isStreamAvailable
  , isIngestActive
  , streamIsAvailable
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map)
import Foreign (Foreign)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub)
import Record as Record
import Rtsv2.Env as Env
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (IpAndPort)
import Serf as Serf
import Shared.Agent as Agent
import Shared.Stream (StreamId(..), StreamVariantId(..))

type State
  = { config :: Config
    , serfRpcAddress :: IpAndPort
    , activeStreams :: Map String Boolean -- TODO
    }

type Config
  = { bindPort :: Int
    , rpcPort :: Int
    }

data Msg
  = JoinAll
  | SerfMessage (Serf.Message StateMessage)

data StateMessage = StreamAvailable String

isStreamAvailable :: StreamId -> Effect Boolean
isStreamAvailable (StreamId s) = Gen.call serverName \state -> CallReply false state

isIngestActive :: StreamVariantId -> Effect Boolean
isIngestActive (StreamVariantId s v) = Gen.call serverName \state -> CallReply false state

streamIsAvailable :: StreamVariantId -> Effect Unit
streamIsAvailable (StreamVariantId s _) =
  Gen.doCast serverName
    $ \state -> do
      _ <- Serf.event state.serfRpcAddress "streamAvailable" (StreamAvailable s) true
      pure $ Gen.CastNoReply state

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

  pure { config
       , serfRpcAddress
       }

logInfo :: forall a b. Nub ( domain :: List Atom | a ) b => String -> Record a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.IntraPoPAgent)) : nil) } { misc: metaData })

handleInfo :: Msg -> State -> Effect State
handleInfo msg state = case msg of
  SerfMessage (Serf.UserEvent name lamportClock coalesce (StreamAvailable streamName)) -> do
    _ <- logInfo "Really Holy cow" {name: name,
                                    payload: streamName}
    pure state


  SerfMessage (Serf.UserEvent name lamportClock coalesce payload) -> do
    _ <- logInfo "Really Holy cow" {name: name,
                                    payload: payload}
    pure state
  SerfMessage a -> do
    _ <- logInfo "Holy cow" {serf: a}
    pure state
  JoinAll -> do
    _ <- joinAllSerf state
    pure state

joinAllSerf :: State -> Effect Unit
joinAllSerf {config, serfRpcAddress}  = do
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
  _ <-
    logInfo "Intra-PoP Agent Starting"
      { config: config
      , seeds: seedAddresses
      }
  result <- Serf.join serfRpcAddress seedAddresses true
  _ <- logInfo "Serf said " { result: result }
  pure unit
