module Rtsv2.Agents.IngestAggregatorInstance
  ( startLink
  , isAvailable
  , addVariant
  , addRemoteVariant
  , removeVariant
  , getState
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map, delete, insert, size, toUnfoldable)
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
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamAndVariant, StreamId(..), StreamVariant, toStreamId, toVariant)
import Shared.Types (IngestAggregatorPublicState, ServerAddress)

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , thisNode :: ServerAddress
    , streamId :: StreamId
    , streamDetails :: StreamDetails
    , activeStreamVariants :: Map StreamVariant ServerAddress
    }

data Msg
  = Tick
  | MaybeStop

isAvailable :: StreamId -> Effect Boolean
isAvailable streamId = Names.isRegistered (serverName streamId)

serverName :: StreamId -> ServerName State Msg
serverName = Names.ingestAggregatorInstanceName

addVariant :: StreamAndVariant -> Effect Unit
addVariant streamAndVariant = Gen.call (serverName (toStreamId streamAndVariant))
  \state@{thisNode, activeStreamVariants} ->
  CallReply unit state{activeStreamVariants = insert (toVariant streamAndVariant) thisNode activeStreamVariants}

addRemoteVariant :: StreamAndVariant -> ServerAddress -> Effect Unit
addRemoteVariant streamAndVariant remoteServer = Gen.call (serverName (toStreamId streamAndVariant))
  \state@{activeStreamVariants} ->
  CallReply unit state{activeStreamVariants = insert (toVariant streamAndVariant) remoteServer activeStreamVariants}

removeVariant :: StreamAndVariant -> Effect Unit
removeVariant streamAndVariant = Gen.doCall (serverName (toStreamId streamAndVariant))
  \state@{activeStreamVariants, streamId, config:{shutdownLingerTimeMs}} -> do
  let
    newActiveStreamVariants = delete (toVariant streamAndVariant) activeStreamVariants
  _ <- if (size newActiveStreamVariants) == 0 then do
         _ <- Timer.sendAfter (serverName streamId) shutdownLingerTimeMs MaybeStop
         pure unit
       else
         pure unit
  pure $ CallReply unit state{activeStreamVariants = newActiveStreamVariants}

getState :: StreamId -> Effect (IngestAggregatorPublicState)
getState streamId = Gen.call (serverName streamId)
  \state@{streamDetails, activeStreamVariants} ->
  CallReply {streamDetails, activeStreamVariants: (\(Tuple streamVariant serverAddress) -> {streamVariant, serverAddress}) <$>(toUnfoldable activeStreamVariants)} state

startLink :: StreamDetails -> Effect StartLinkResult
startLink streamDetails@{slot : {name}} = Gen.startLink (serverName (StreamId name)) (init streamDetails) handleInfo

init :: StreamDetails -> Effect State
init streamDetails = do
  _ <- logInfo "Ingest Aggregator starting" {streamId: streamId}
  config <- Config.ingestAggregatorAgentConfig
  thisNode <- PoPDefinition.thisNode
  _ <- Timer.sendEvery (serverName streamId) config.streamAvailableAnnounceMs Tick
  _ <- announceStreamIsAvailable streamId
  pure { config : config
       , thisNode
       , streamId
       , streamDetails
       , activeStreamVariants : Map.empty
       }
  where
    streamId = StreamId streamDetails.slot.name

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{activeStreamVariants, streamId} =
  case msg of
    Tick -> CastNoReply <$> handleTick state
    MaybeStop
      | size activeStreamVariants == 0 -> do
        _ <- logInfo "Ingest Aggregator stopping" {streamId: streamId}
        _ <- announceStreamStopped streamId
        pure $ CastStop state
      | otherwise -> pure $ CastNoReply state

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
