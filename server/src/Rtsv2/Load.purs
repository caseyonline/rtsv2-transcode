module Rtsv2.Load
       ( startLink
       , load
       , setLoad
       ) where

import Prelude

import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Foreign (Foreign)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Record as Record
import Rtsv2.Config as Config
import Rtsv2.IntraPoPAgent as IntraPopAgent
import Shared.Types (Load)

type State =
  {
    load :: Load
  }

data Msg = Tick

serverName :: ServerName State Msg
serverName = Local "load"

startLink :: Unit -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

load :: Effect Load
load =
  Gen.call serverName \state@{ load: currentLoad } -> CallReply currentLoad state

setLoad :: Number -> Effect Unit
setLoad newLoad = do
  _ <- IntraPopAgent.announceLoad (wrap newLoad)
  Gen.call serverName \state -> CallReply unit state{load = (wrap newLoad)}

init :: Unit -> Effect State
init args = do
  _ <- logInfo "Load monitor starting" {}
  config <- Config.loadMonitorConfig
  _ <- Timer.sendEvery serverName config.loadAnnounceMs Tick
  pure $ {load: (wrap 0.0)}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{load: currentLoad} =
  case msg of
    Tick ->
      do
        _ <- IntraPopAgent.announceLoad currentLoad
        pure $ CastNoReply state

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom "LoadMonitor") : nil) } { misc: metaData })
