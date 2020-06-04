module Rtsv2.Alerts
       ( startLink
       , currentAlerts
       ) where

import Prelude

import Bus as Bus
import Data.Filterable (filter)
import Data.Long as Long
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, reverse, singleton, sortBy)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Utils as Erl
import Foreign (Foreign)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Shared.Common (Alert(..), AlertData(..), LoggingContext, LoggingSource, Milliseconds)

bus :: Bus.Bus Atom Foreign
bus = Bus.bus (atom "rtsv2_alerts")

data AlertKey = Key { pid :: Maybe String
                    , source :: Maybe LoggingSource
                    , context :: Maybe LoggingContext
                    , alertData :: Maybe AlertData
                    }
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

currentAlerts :: Maybe Milliseconds -> Effect (List Alert)
currentAlerts maybeFrom =
  Gen.call serverName (\state@{alerts} ->
                        let
                          from = fromMaybe mempty maybeFrom

                          filteredSorted = reverse
                                           $ sortBy (\(Alert {lastReport: lhs}) (Alert {lastReport: rhs}) -> compare lhs rhs)
                                           $ filter (\(Alert {lastReport}) -> lastReport > from)
                                           $ Map.values alerts

                        in
                          CallReply filteredSorted state
                      )

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
    alert = mapForeign f
    alerts2 = Map.alter (updateMap alert) (alertToKey alert) alerts
  pure $ state{alerts = alerts2}

  where
    updateMap :: Alert -> Maybe Alert -> Maybe Alert
    updateMap alert Nothing = Just alert
    updateMap (Alert alert) (Just (Alert existing@{initialReport, repeatCount})) = Just (Alert alert{ initialReport = initialReport
                                                                                                    , repeatCount = repeatCount + 1})

doExpiry :: State -> Effect State
doExpiry state@{alerts, alertRetention} = do
  now <- Erl.systemTimeMs
  let
    expire = now - alertRetention
    alerts2 = Map.filter (\(Alert {lastReport}) -> lastReport > expire) alerts
  pure state{alerts = alerts2}

alertToKey :: Alert -> AlertKey
alertToKey alert@(Alert {alert: alertData}) =
  alertDataToKey alertData alert

alertDataToKey :: AlertData -> Alert -> AlertKey
alertDataToKey (GenericAlert _) alert = defaultAlertKey alert Nothing

alertDataToKey IngestStarted alert = defaultAlertKey alert Nothing

alertDataToKey alertData@(IngestFailed reason) alert = defaultAlertKey alert (Just alertData)

alertDataToKey alertData@(LSRSFailed reason) alert = defaultAlertKey alert (Just alertData)

defaultAlertKey :: Alert -> Maybe AlertData -> AlertKey
defaultAlertKey (Alert { source: Nothing }) _ = NoKey

defaultAlertKey (Alert { context
                       , source
                       , pid}) alertData = Key { pid: maybe (Just pid) (const Nothing) context
                                               , context
                                               , source
                                               , alertData
                                               }

domain :: List Atom
domain = (atom "Alerts") # singleton

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo = Logger.info <<< Logger.traceMetadata domain
