module Rtsv2.Agents.IngestAggregatorInstance
  ( startLink
  , isAvailable
  , addVariant
  , getState
  , stopAggregator
  ) where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, toUnfoldable, (:))
import Erl.Data.Map (Map, insert, keys)
import Erl.Data.Map as Map
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP (announceStreamIsAvailable, announceStreamStopped)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Shared.Agent as Agent
import Shared.Stream (StreamAndVariant, StreamId, toStreamId)

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , streamId :: StreamId
    , streamVariants :: Map StreamAndVariant Unit
    }

data Msg
  = Tick

isAvailable :: StreamId -> Effect Boolean
isAvailable streamId = Names.isRegistered (serverName streamId)

serverName :: StreamId -> ServerName State Msg
serverName = Names.ingestAggregatorInstanceName

addVariant :: StreamAndVariant -> Effect Unit
addVariant streamVariantId = Gen.call (serverName (toStreamId streamVariantId))
  \state@{streamVariants} ->
  CallReply unit state{streamVariants = insert streamVariantId unit streamVariants}

-- TODO - would rather a list, but it doesn't call writeForeign on StreamVariantId
getState :: StreamId -> Effect (Array StreamAndVariant)
getState streamId = Gen.call (serverName streamId)
  \state@{streamVariants} ->
  CallReply (toUnfoldable (keys streamVariants)) state

stopAggregator :: StreamId -> Effect Unit
stopAggregator streamId = do
  Gen.doCall (serverName streamId) \state -> do
    _ <- logInfo "Ingest Aggregator stopping" {streamId: streamId}
    _ <- announceStreamStopped streamId
    pure $ CallStop unit state

startLink :: StreamId -> Effect StartLinkResult
startLink streamId = Gen.startLink (serverName streamId) (init streamId) handleInfo

init :: StreamId -> Effect State
init streamId = do
  _ <- logInfo "Ingest Aggregator starting" {streamId: streamId}
  config <- Config.ingestAggregatorAgentConfig
  _ <- Timer.sendEvery (serverName streamId) config.streamAvailableAnnounceMs Tick
  _ <- announceStreamIsAvailable streamId

  pure { config : config
       , streamId
       , streamVariants : Map.empty
       }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  case msg of
    Tick -> CastNoReply <$> handleTick state

handleTick :: State -> Effect State
handleTick state@{streamId} = do
  _ <- announceStreamIsAvailable streamId
  pure state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.IngestAggregator :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
