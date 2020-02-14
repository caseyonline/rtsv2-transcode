module Rtsv2.Agents.IngestStats
  ( startLink
  , getStats
  , getStatsForIngest
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Data.List (List, filter, head, nil)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Shared.Stream (IngestKey)
import Shared.Types.Agent.State (IngestStats)

type IngestStats' = IngestStats List

foreign import getStatsImpl :: Effect (List IngestStats')

data Msg = Tick

type State =
  { config :: Config.IngestStatsConfig
  , currentStats :: (List IngestStats')
  }

serverName :: ServerName State Msg
serverName = Names.ingestStatsName

startLink :: Unit -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

getStats :: Effect (List IngestStats')
getStats =
  Gen.call serverName \state@{currentStats} -> CallReply currentStats state

getStatsForIngest :: IngestKey -> Effect (Maybe IngestStats')
getStatsForIngest requestedIngestKey =
  Gen.call serverName (\state@{currentStats} ->
                        let
                          result = head $ filter (\{ingestKey} -> ingestKey == requestedIngestKey) currentStats
                        in
                         CallReply result state)

init :: Unit -> Effect State
init _ = do
  config@{pollPeriodMs} <- Config.ingestStatsConfig
  void $ Timer.sendEvery serverName pollPeriodMs Tick
  pure $ { config
         , currentStats: nil}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = do
  case msg of
    Tick -> do
      newState <- configPoll state
      pure $ CastNoReply newState

configPoll :: State -> Effect State
configPoll state = do
  stats <- getStatsImpl
  pure state{currentStats = stats}
