module Rtsv2.Agents.IngestInstance
  ( startLink
  , isActive
  , stopIngest
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Utils (Milliseconds)
import Rtsv2.Names as Names
import Logger (Logger, spy)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..))
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Audit as Audit
import Rtsv2.Config as Config
import Rtsv2.Load as Load
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..))
import Rtsv2.Router.Endpoint as RoutingEndpoint
import Rtsv2.Router.Parser as Routing
import Shared.Agent as Agent
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamAndVariant, StreamId, toStreamId, toVariant)
import Shared.Types (Load, Server, ServerLoad(..), extractAddress, serverLoadToServer)
import Simple.JSON as JSON
import SpudGun as SpudGun

serverName :: StreamAndVariant -> ServerName State Msg
serverName streamAndVariant = Names.ingestInstanceName streamAndVariant

data Msg
   = InformAggregator
   | IntraPoPBus IntraPoP.IntraPoPBusMessage

type State
  = { thisServer :: Server
    , aggregatorRetryTime :: Milliseconds
    , streamAndVariant :: StreamAndVariant
    , streamDetails :: StreamDetails
    , aggregatorAddr :: Maybe Server
    }

startLink :: Tuple StreamDetails StreamAndVariant -> Effect StartLinkResult
startLink (Tuple streamDetails streamAndVariant) = Gen.startLink (serverName streamAndVariant) (init streamDetails streamAndVariant) handleInfo

isActive :: StreamAndVariant -> Effect Boolean
isActive streamAndVariant = Names.isRegistered (serverName streamAndVariant)

stopIngest :: StreamAndVariant -> Effect Unit
stopIngest streamAndVariant =
  Gen.doCall (serverName streamAndVariant) \state@{thisServer, aggregatorAddr} -> do
    _ <- removeVariant thisServer streamAndVariant aggregatorAddr
    _ <- Audit.ingestStop streamAndVariant
    pure $ CallStop unit state

init :: StreamDetails -> StreamAndVariant -> Effect State
init streamDetails streamAndVariant = do
  _ <- logInfo "Ingest starting" {streamAndVariant: streamAndVariant}
  thisServer <- PoPDefinition.getThisServer
  {intraPoPLatencyMs} <- Config.globalConfig
  _ <- Bus.subscribe (serverName streamAndVariant) IntraPoP.bus IntraPoPBus
  _ <- Audit.ingestStart streamAndVariant
  _ <- Timer.sendAfter (serverName streamAndVariant) 0 InformAggregator
  pure { thisServer
       , streamDetails
       , streamAndVariant
       , aggregatorRetryTime: wrap intraPoPLatencyMs
       , aggregatorAddr: Nothing
       }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = case msg of
  InformAggregator -> do
    state2 <- informAggregator state
    pure $ CastNoReply state2
  IntraPoPBus (IngestAggregatorExited streamId serverAddress) -> do
    _ <- logInfo "exit" {streamId, serverAddress, thisStreamId: state.streamAndVariant}
    state2 <- handleAggregatorExit streamId serverAddress state
    pure $ CastNoReply state2

informAggregator :: State -> Effect State
informAggregator state@{streamDetails, streamAndVariant, thisServer, aggregatorRetryTime} = do
  maybeAggregator <- getAggregator streamDetails streamAndVariant
  maybeVariantAdded <- sequence ((addVariant thisServer streamAndVariant) <$> maybeAggregator)
  case fromMaybe false maybeVariantAdded of
    true -> pure state{aggregatorAddr = maybeAggregator}
    false -> do
      _ <- Timer.sendAfter (serverName streamAndVariant) (unwrap aggregatorRetryTime) InformAggregator
      pure state

handleAggregatorExit :: StreamId -> Server -> State -> Effect State
handleAggregatorExit exitedStreamId exitedAggregatorAddr state@{streamAndVariant, aggregatorRetryTime, aggregatorAddr}
  | exitedStreamId == (toStreamId streamAndVariant) && Just exitedAggregatorAddr == aggregatorAddr = do
      _ <- Timer.sendAfter (serverName streamAndVariant) 0 InformAggregator
      pure state
  | otherwise =
      pure state

addVariant :: Server -> StreamAndVariant -> Server -> Effect Boolean
addVariant thisServer streamAndVariant aggregatorAddress
  | aggregatorAddress == thisServer = do
    _ <- IngestAggregatorInstance.addVariant streamAndVariant
    pure true
  | otherwise = do
    let
      -- TODO - functions to make URLs from Server
      url = makeActiveIngestUrl aggregatorAddress streamAndVariant
    restResult <- SpudGun.postJson (wrap url) $ extractAddress thisServer
    case restResult of
      Left _ -> pure $ false
      Right _ -> pure $ true


makeActiveIngestUrl :: Server -> StreamAndVariant -> String
makeActiveIngestUrl server streamAndVariant =
  "http://" <> (unwrap $ extractAddress server) <> ":3000/api/agents/ingestAggregator/"
            <> (unwrap $ toStreamId streamAndVariant) <> "/activeIngests/" <> (unwrap $ toVariant streamAndVariant)


removeVariant :: Server -> StreamAndVariant -> Maybe Server-> Effect Unit
removeVariant thisServer streamAndVariant Nothing = pure unit
removeVariant thisServer streamAndVariant (Just aggregatorAddress)
  | aggregatorAddress == thisServer = do
    _ <- IngestAggregatorInstance.removeVariant streamAndVariant
    pure unit
  | otherwise = do
    let
      url = makeActiveIngestUrl aggregatorAddress streamAndVariant
      -- TODO - functions to make URLs from Server
    restResult <- SpudGun.delete (wrap url) {}
    case (spy "result" restResult) of
      Left _ -> pure $ unit
      Right _ -> pure $ unit

getAggregator :: StreamDetails -> StreamAndVariant -> Effect (Maybe Server)
getAggregator streamDetails streamAndVariant = do
  maybeAggregator <- IntraPoP.whereIsIngestAggregator (toStreamId streamAndVariant)
  case maybeAggregator of
    Just server ->
      pure $ Just server
    Nothing ->
      launchLocalOrRemote streamDetails streamAndVariant

launchLocalOrRemote :: StreamDetails -> StreamAndVariant -> Effect (Maybe Server)
launchLocalOrRemote streamDetails streamAndVariant = do
  currentLoad <- Load.load
  if
    currentLoad < loadThresholdToCreateAggregator then do
      _ <- IngestAggregatorInstanceSup.startAggregator streamDetails
      Just <$> PoPDefinition.getThisServer
    else
      launchRemote streamDetails streamAndVariant

-- todo - maybe move to intrapop so we know if a second aggregator requests comes in quickly
-- it could serialise requests on a per stream / asset basis
launchRemote :: StreamDetails -> StreamAndVariant -> Effect (Maybe Server)
launchRemote streamDetails streamAndVariant = do
  candidate <- IntraPoP.getIdleServer filterForAggregatorLoad
  case candidate of
    Nothing ->
      pure Nothing
    Just server -> do
      let
        path = Routing.printUrl RoutingEndpoint.endpoint IngestAggregatorsE
        url = spy "url" $ "http://" <> toHost server <> ":3000" <> path

        -- TODO - functions to make URLs from ServerAddress
        --url = "http://" <> (unwrap $ extractAddress server) <> ":3000/api/agents/ingestAggregator"
      restResult <- SpudGun.postJson (wrap url) streamDetails
      case restResult of
        Left _ -> pure Nothing
        Right _ -> pure $ serverLoadToServer <$> candidate

loadThresholdToCreateAggregator :: Load
loadThresholdToCreateAggregator = wrap 50.0

filterForAggregatorLoad :: ServerLoad -> Boolean
filterForAggregatorLoad (ServerLoad sl) = sl.load < loadThresholdToCreateAggregator

toHost :: ServerLoad -> String
toHost =
  unwrap <<< extractAddress


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.Ingest :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
