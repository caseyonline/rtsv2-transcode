module Rtsv2.Load
       ( startLink
       , load
       , setLoad
       ) where

import Prelude

import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, singleton)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP as IntraPop
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Shared.Types (Load)

type State =
  {
    load :: Load
  }

data Msg = Tick

serverName :: ServerName State Msg
serverName = Names.loadServerName

startLink :: Unit -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

load :: Effect Load
load =
  Gen.call serverName \state@{ load: currentLoad } -> CallReply currentLoad state

setLoad :: Number -> Effect Unit
setLoad newLoad = do
  _ <- IntraPop.announceLoad (wrap newLoad)
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
        _ <- IntraPop.announceLoad currentLoad
        pure $ CastNoReply state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = serverName # Names.toDomain # singleton

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
