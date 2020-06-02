module Rtsv2.Alerts
       ( startLink
       , currentAlerts
       ) where

import Prelude

import Bus as Bus
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil)
import Foreign (Foreign)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CastResult(..))
import Pinto.Gen as Gen
import Rtsv2.Names as Names

class Alert alert where
  toJson :: alert -> Foreign

bus :: Bus.Bus Atom Foreign
bus = Bus.bus (atom "rtsv2_alerts")

type State alert =
  { alerts :: List alert
  }

data Msg = BusMsg Foreign

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
startLink :: Unit -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

currentAlerts :: forall alert. Effect (List alert)
currentAlerts =
  pure nil

------------------------------------------------------------------------------
-- gen_server callbacks
------------------------------------------------------------------------------
init :: forall alert. Unit -> Effect (State alert)
init args = do
  void $ Bus.subscribe serverName bus BusMsg
  pure { alerts : nil}

handleInfo :: forall alert. Msg -> (State alert) -> Effect (CastResult (State alert))
handleInfo msg state =
  case msg of
    BusMsg f -> do
      -- let
      --   _ = spy "GOT" {f}
      pure $ CastNoReply state

------------------------------------------------------------------------------
-- Internal functions
------------------------------------------------------------------------------
serverName :: forall alert. ServerName (State alert) Msg
serverName = Names.alertsServerName
