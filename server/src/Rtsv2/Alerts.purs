module Rtsv2.Alerts
       ( startLink
       , currentAlerts
       ) where

import Prelude

import Bus as Bus
import Data.Long as Long
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Utils as Erl
import Foreign (Foreign)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Shared.Common (Alert(..), LoggingMetadata, LoggingSource, Milliseconds)

bus :: Bus.Bus Atom Foreign
bus = Bus.bus (atom "rtsv2_alerts")

-- TODO - include data in the key? Maybe just have Eq defined on Alert and use a set?
data AlertKey = ByPid LoggingSource String
              | ByMetadata LoggingSource LoggingMetadata
              | NoKey

type State =
  { alertRetention :: Milliseconds
  , alerts :: Map AlertKey Alert
  }

data Msg = Expire
         | BusMsg Foreign

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
startLink :: Unit -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

currentAlerts :: Effect (List Alert)
currentAlerts =
  Gen.call serverName (\state@{alerts} -> CallReply (Map.values alerts) state)

------------------------------------------------------------------------------
-- gen_server callbacks
------------------------------------------------------------------------------
init :: Unit -> Effect State
init args = do
  { alertRetentionMs } <- Config.alertConfig
  void $ Bus.subscribe serverName bus BusMsg
  void $ Timer.sendEvery serverName 1000 Expire
  pure { alerts : Map.empty
       , alertRetention : wrap $ Long.fromInt alertRetentionMs
       }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{alerts} =
  case msg of
    Expire ->
      CastNoReply <$> doExpiry state

    BusMsg f ->
      CastNoReply <$> addOrUpdateAlert f state

------------------------------------------------------------------------------
-- Internal functions
------------------------------------------------------------------------------
foreign import mapForeign :: Foreign -> Alert

serverName :: ServerName State Msg
serverName = Names.alertsServerName

addOrUpdateAlert :: Foreign -> State -> Effect State
addOrUpdateAlert f state@{alerts} = do
  let
    alert = mapForeign (spy "in" f)
    alerts2 = Map.alter (updateMap alert) (alertKey alert) alerts
    _ = spy "GOT" alert
  pure $ state{alerts = alerts2}

  where
    updateMap :: Alert -> Maybe Alert -> Maybe Alert
    updateMap alert Nothing = Just alert
    updateMap (Alert alert) (Just (Alert existing@{initialReport, repeatCount})) = Just (Alert alert{ initialReport = initialReport
                                                                                                    , repeatCount = repeatCount + 1})

    alertKey (Alert { source: Nothing }) = NoKey

    alertKey (Alert { metadata: Nothing
                    , source: Just source
                    , pid}) = ByPid source pid

    alertKey (Alert { metadata: Just metadata
                    , source: Just source}) = ByMetadata source metadata

doExpiry :: State -> Effect State
doExpiry state@{alerts, alertRetention} = do
  now <- Erl.systemTimeMs
  let
    expire = now - alertRetention
    alerts2 = Map.filter (\(Alert {lastReport}) -> lastReport > expire) alerts
  pure state{alerts = alerts2}
