module Rtsv2.Agents.IngestInstance
  ( startLink
  , isActive
  , stopIngest
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Utils (Milliseconds)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto as Pinto
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IngestAggregatorInstance as IngestAggregatorInstance
import Rtsv2.Agents.IngestAggregatorInstanceSup as IngestAggregatorInstanceSup
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..), launchLocalOrRemoteGeneric)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator (extractServer)
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), ResourceResp)
import Rtsv2.Audit as Audit
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Rtsv2.Utils (crashIfLeft)
import Shared.Agent as Agent
import Shared.LlnwApiTypes (StreamDetails)
import Shared.Stream (StreamAndVariant, StreamId, toStreamId, toVariant)
import Shared.Types (Load, Server, ServerLoad(..), extractAddress)
import SpudGun (Url)
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
    , aggregatorAddr :: Maybe (LocalOrRemote Server)
    }

startLink :: Tuple StreamDetails StreamAndVariant -> Effect StartLinkResult
startLink (Tuple streamDetails streamAndVariant) = Gen.startLink (serverName streamAndVariant) (init streamDetails streamAndVariant) handleInfo

isActive :: StreamAndVariant -> Effect Boolean
isActive streamAndVariant = Pinto.isRegistered (serverName streamAndVariant)

stopIngest :: StreamAndVariant -> Effect Unit
stopIngest streamAndVariant =
  Gen.doCall (serverName streamAndVariant) \state@{aggregatorAddr} -> do
    _ <- removeVariant streamAndVariant aggregatorAddr
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
  maybeAggregator <- hush <$> getAggregator streamDetails streamAndVariant
  maybeVariantAdded <- sequence ((addVariant thisServer streamAndVariant) <$> (extractServer <$>  maybeAggregator))
  case fromMaybe false maybeVariantAdded of
    true -> pure state{aggregatorAddr = maybeAggregator}
    false -> do
      _ <- Timer.sendAfter (serverName streamAndVariant) (unwrap aggregatorRetryTime) InformAggregator
      pure state

handleAggregatorExit :: StreamId -> Server -> State -> Effect State
handleAggregatorExit exitedStreamId exitedAggregatorAddr state@{streamAndVariant, aggregatorRetryTime, aggregatorAddr}
  | exitedStreamId == (toStreamId streamAndVariant) && Just exitedAggregatorAddr == (extractServer <$> aggregatorAddr) = do
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
    restResult <- SpudGun.postJson url $ extractAddress thisServer
    case restResult of
      Left _ -> pure $ false
      Right _ -> pure $ true

makeActiveIngestUrl :: Server -> StreamAndVariant -> Url
makeActiveIngestUrl server streamAndVariant =
  makeUrl server $ IngestAggregatorActiveIngestsE (toStreamId streamAndVariant) (toVariant streamAndVariant)

removeVariant :: StreamAndVariant -> Maybe (LocalOrRemote Server)-> Effect Unit
removeVariant streamAndVariant Nothing = pure unit
removeVariant streamAndVariant (Just (Local aggregator)) = do
    _ <- IngestAggregatorInstance.removeVariant streamAndVariant
    pure unit
removeVariant streamAndVariant (Just (Remote aggregator)) = do
  let
    url = makeActiveIngestUrl aggregator streamAndVariant
  void $ crashIfLeft =<< SpudGun.delete url {}

getAggregator :: StreamDetails -> StreamAndVariant -> Effect (ResourceResp Server)
getAggregator streamDetails streamAndVariant = do
  maybeAggregator <- IntraPoP.whereIsIngestAggregator (toStreamId streamAndVariant)
  case maybeAggregator of
    Just server ->
      pure $ Right $ Local server
    Nothing ->
      launchLocalOrRemote streamDetails streamAndVariant

launchLocalOrRemote :: StreamDetails -> StreamAndVariant -> Effect (ResourceResp Server)
launchLocalOrRemote streamDetails streamAndVariant = do
  launchLocalOrRemoteGeneric filterForAggregatorLoad launchLocal launchRemote
  where
    launchLocal :: ServerLoad -> Effect Unit
    launchLocal _ = do
      void $ IngestAggregatorInstanceSup.startAggregator streamDetails
      pure unit
    launchRemote idleServer = do
      let
        url = makeUrl idleServer IngestAggregatorsE
      void $ crashIfLeft =<< SpudGun.postJson url streamDetails


loadThresholdToCreateAggregator :: Load
loadThresholdToCreateAggregator = wrap 50.0

filterForAggregatorLoad :: ServerLoad -> Boolean
filterForAggregatorLoad (ServerLoad sl) = sl.load < loadThresholdToCreateAggregator

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
