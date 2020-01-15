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
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Data.Map (Map, delete, insert, keys, size, toUnfoldable)
import Erl.Data.Map as Map
import Foreign (Foreign)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Record as Record
import Rtsv2.Agents.IntraPoP (announceStreamIsAvailable, announceStreamStopped)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Shared.Agent as Agent
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamId(..), StreamVariantId, toStreamId)
import Shared.Types (IngestAggregatorPublicState, ServerAddress)

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , thisNode :: ServerAddress
    , streamId :: StreamId
    , streamDetails :: StreamDetails
    , activeStreamVariants :: Map StreamVariantId ServerAddress
    }

data Msg
  = Tick
  | MaybeStop

isAvailable :: StreamId -> Effect Boolean
isAvailable streamId = Names.isRegistered (serverName streamId)

serverName :: StreamId -> ServerName State Msg
serverName = Names.ingestAggregatorInstanceName

addVariant :: StreamVariantId -> Effect Unit
addVariant streamVariantId = Gen.call (serverName (toStreamId streamVariantId))
  \state@{thisNode, activeStreamVariants} ->
  CallReply unit state{activeStreamVariants = insert streamVariantId thisNode activeStreamVariants}

addRemoteVariant :: StreamVariantId -> ServerAddress -> Effect Unit
addRemoteVariant streamVariantId remoteServer = Gen.call (serverName (toStreamId streamVariantId))
  \state@{activeStreamVariants} ->
  CallReply unit state{activeStreamVariants = insert streamVariantId remoteServer activeStreamVariants}

removeVariant :: StreamVariantId -> Effect Unit
removeVariant streamVariantId = Gen.doCall (serverName (toStreamId streamVariantId))
  \state@{activeStreamVariants, streamId, config:{shutdownLingerTimeMs}} -> do
  let
    newActiveStreamVariants = delete streamVariantId activeStreamVariants
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

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.IngestAggregator)) : nil) } { misc: metaData })
