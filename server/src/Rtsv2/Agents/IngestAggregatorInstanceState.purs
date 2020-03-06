module Rtsv2.Agents.IngestAggregatorInstanceState
       ( startLink
       , serverName
       , Msg
       , StateServerName
       , State
       , registerStartup
       , recordInstanceData
       ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process.Raw (Pid)
import Erl.Utils as Erl
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Rtsv2.Names as Names

data Msg = InstanceDown Pid

type State instanceKey instanceData =
  { activeInstanceData :: Map instanceKey instanceData
  , activeInstances :: Map Pid instanceKey
  }

type StateServerName instanceKey instanceData = ServerName (State instanceKey instanceData) Msg

serverName :: forall instanceKey instanceData. StateServerName instanceKey instanceData
serverName = Names.ingestAggregatorInstanceStateName -- name passed in?  or is this just global?

startLink :: forall instanceKey instanceData. StateServerName instanceKey instanceData -> Effect StartLinkResult
startLink thisServerName = Gen.startLink thisServerName init handleInfo

registerStartup :: forall instanceKey instanceData. instanceKey -> Effect (Maybe instanceData)
registerStartup instanceKey = do
  instancePid <- Erl.self
  Gen.doCall serverName (doRegisterStartup instancePid)
  where
    doRegisterStartup instancePid state@{activeInstanceData, activeInstances} = do
      let
        thisInstanceData = Map.lookup instanceKey activeInstanceData
        newActiveInstances = Map.insert instancePid instanceKey activeInstances
      Gen.monitorPid serverName instancePid (\_ -> InstanceDown instancePid)
      pure $ CallReply thisInstanceData state{activeInstances = newActiveInstances}

recordInstanceData :: forall instanceKey instanceData. instanceKey -> instanceData -> Effect Unit
recordInstanceData instanceKey instanceData = do
  Gen.call serverName doRecordInstanceData
  where
    doRecordInstanceData state@{activeInstanceData} =
      let
        newActiveInstanceData = Map.insert instanceKey instanceData activeInstanceData
      in
      CallReply unit state{activeInstanceData = newActiveInstanceData}

init :: forall instanceKey instanceData. Effect (State instanceKey instanceData)
init =
  pure { activeInstanceData: Map.empty
       , activeInstances: Map.empty
       }

handleInfo :: forall instanceKey instanceData. Msg -> State instanceKey instanceData -> Effect (CastResult (State instanceKey instanceData))
handleInfo msg state = case msg of
  InstanceDown pid ->

    pure $ CastNoReply state


-- record active by slotid, slotrole
-- record instance by pid -> slotid, slotrole
-- if instance exits, then can start timer to purge that data
-- on startup, instance requests
