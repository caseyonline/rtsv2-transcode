module Rtsv2.Agents.IngestStats
  ( startLink
  , getStats
  ) where

import Prelude

import Effect (Effect)
import Erl.Data.List (List, nil)
import Logger (spy)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Shared.Types.Agent.State (IngestStats)

type IngestStatsList = IngestStats List

foreign import getStatsImpl :: Effect IngestStatsList

data Msg = Tick

type State =
  { config :: Config.IngestStatsConfig
  , currentStats :: IngestStatsList
  }

serverName :: ServerName State Msg
serverName = Names.ingestStatsName

startLink :: Unit -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

getStats :: Effect IngestStatsList
getStats = Gen.call serverName \state@{currentStats} -> CallReply currentStats state

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
