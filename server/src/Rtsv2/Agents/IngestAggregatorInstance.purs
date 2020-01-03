module Rtsv2.Agents.IngestAggregatorInstance
  ( startLink
  , stopAggregator
  ) where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
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
import Shared.Agent as Agent
import Shared.Stream (StreamId)

type State
  = { config :: Config.IngestAggregatorAgentConfig
    , streamId :: StreamId
    }

data Msg
  = Tick

serverName :: StreamId -> ServerName State Msg
serverName = Names.ingestAggregatorInstanceName

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
       }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  case msg of
    Tick -> CastNoReply <$> handleTick state

handleTick :: State -> Effect State
handleTick state@{streamId} = do
  _ <- announceStreamIsAvailable streamId
  pure state

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.IngestAggregator)) : nil) } { misc: metaData })
