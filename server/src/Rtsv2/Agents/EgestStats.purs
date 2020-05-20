module Rtsv2.Agents.EgestStats
  ( startLink
  , getStats
  ) where

import Prelude

import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, singleton)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Config (EgestStatsConfig)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.JsonLd (EgestStats)
import Shared.Rtsv2.Stream (EgestKey)

type EgestStats' = EgestStats List

foreign import getEgestKeys :: Effect (List EgestKey)

data Msg = Tick

type State =
  { config :: EgestStatsConfig
  , currentStats :: (List EgestStats')
  }

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
startLink :: Unit -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

getStats :: Effect (List EgestStats')
getStats =
  Gen.call serverName \state@{currentStats} -> CallReply currentStats state

------------------------------------------------------------------------------
-- gen_server callbacks
------------------------------------------------------------------------------
init :: Unit -> Effect State
init _ = do
  config@{pollPeriodMs} <- Config.egestStatsConfig
  void $ Timer.sendEvery serverName pollPeriodMs Tick
  pure $ { config
         , currentStats: nil}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = do
  case msg of
    Tick -> do
      newState <- gatherStats state
      pure $ CastNoReply newState

------------------------------------------------------------------------------
-- Internal functions
------------------------------------------------------------------------------
serverName :: ServerName State Msg
serverName = Names.egestStatsName

gatherStats :: State -> Effect State
gatherStats state = do
  keys <- getEgestKeys
  stats <- traverse EgestInstance.currentStats keys
  pure state{currentStats = stats}

domain :: List Atom
domain = Agent.Egest # show # atom # singleton

logInfo :: forall a. Logger (Record a)
logInfo = Logger.doLog domain Logger.info
