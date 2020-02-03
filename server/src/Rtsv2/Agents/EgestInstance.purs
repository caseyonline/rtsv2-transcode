module Rtsv2.Agents.EgestInstance
  ( startLink
  , isActive
  , addClient
  , removeClient
  , currentStats
  , CreateEgestPayload
  ) where

import Prelude

import Bus as Bus
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, singleton)
import Erl.Utils (Milliseconds, Ref, makeRef)
import Logger (Logger, spy)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto as Pinto
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Agents.IntraPoP (IntraPoPBusMessage(..), launchLocalOrRemoteGeneric)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator.Types (LocalOrRemote(..), RegistrationResp, ResourceResp)
import Rtsv2.Agents.StreamRelay.Instance as StreamRelayInstance
import Rtsv2.Agents.StreamRelay.InstanceSup (startRelay) as StreamRelayInstanceSup
import Rtsv2.Agents.StreamRelay.Types (CreateRelayPayload, RegisterEgestPayload)
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Rtsv2.Utils (crashIfLeft)
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (EgestServer, Load, PoPName, RelayServer, Server, ServerLoad(..))
import Shared.Types.Agent.State as PublicState
import SpudGun as SpudGun

type CreateEgestPayload
  = { streamId :: StreamId
    , aggregatorPoP :: PoPName
    }

type State
  = { streamId :: StreamId
    , aggregatorPoP :: PoPName
    , thisServer :: EgestServer
    , relay :: Maybe (LocalOrRemote RelayServer)
    , clientCount :: Int
    , lingerTime :: Milliseconds
    , relayCreationRetry :: Milliseconds
    , stopRef :: Maybe Ref
    }

data Msg = Tick
         | InitStreamRelays
         | MaybeStop Ref
         | IntraPoPBus IntraPoP.IntraPoPBusMessage

serverName :: StreamId -> ServerName State Msg
serverName streamId = Names.egestInstanceName streamId

isActive :: StreamId -> Effect Boolean
isActive streamId = Pinto.isRegistered (serverName streamId)

startLink :: CreateEgestPayload -> Effect StartLinkResult
startLink payload = Gen.startLink (serverName payload.streamId) (init payload) handleInfo

addClient :: StreamId -> Effect RegistrationResp
addClient streamId = do
  Gen.doCall (serverName streamId) doAddClient

doAddClient :: State -> Effect (CallResult RegistrationResp State)
doAddClient state@{clientCount} = do
  _ <- logInfo "Add client" {newCount: clientCount + 1}
  pure $ CallReply (Right unit) state{ clientCount = clientCount + 1
                                     , stopRef = Nothing
                                     }

removeClient :: StreamId -> Effect Unit
removeClient streamId = Gen.doCall (serverName streamId) doRemoveClient

doRemoveClient :: State -> Effect (CallResult Unit State)
doRemoveClient state@{clientCount: 0} = do
  _ <- logInfo "Remove client - already zero" {}
  pure $ CallReply unit state
doRemoveClient state@{clientCount: 1, lingerTime, streamId} = do
  ref <- makeRef
  _ <- logInfo "Last client gone, start stop timer" {}
  _ <- Timer.sendAfter (serverName streamId) (unwrap lingerTime) (MaybeStop ref)
  pure $ CallReply unit state{ clientCount = 0
                             , stopRef = Just ref
                             }

doRemoveClient state@{clientCount} = do
  _ <- logInfo "Remove client" { newCount: clientCount - 1 }
  pure $ CallReply unit state{ clientCount = clientCount - 1 }

currentStats :: StreamId -> Effect PublicState.Egest
currentStats streamId =
  Gen.call (serverName streamId) \state@{clientCount} ->
    CallReply {clientCount} state


toEgestServer :: Server -> EgestServer
toEgestServer = unwrap >>> wrap

init :: CreateEgestPayload -> Effect State
init payload@{streamId, aggregatorPoP} = do
  _ <- logInfo "Egest starting" {payload}
  _ <- Bus.subscribe (serverName streamId) IntraPoP.bus IntraPoPBus
  {egestAvailableAnnounceMs, lingerTimeMs, relayCreationRetryMs} <- Config.egestAgentConfig

  thisServer <- PoPDefinition.getThisServer
  _ <- IntraPoP.announceEgestIsAvailable streamId
  _ <- Timer.sendEvery (serverName streamId) egestAvailableAnnounceMs Tick
  _ <- Timer.sendAfter (serverName streamId) 0 InitStreamRelays
  maybeRelay <- IntraPoP.whereIsStreamRelay streamId
  let
    state ={ streamId
           , aggregatorPoP
           , thisServer : toEgestServer thisServer
           , relay : Nothing
           , clientCount: 0
           , lingerTime: wrap lingerTimeMs
           , relayCreationRetry : wrap relayCreationRetryMs
           , stopRef: Nothing
           }
  case maybeRelay of
    Just relay ->
      pure state
    Nothing -> do
      -- Launch
      _ <- StreamRelayInstanceSup.startRelay {streamId, aggregatorPoP}
      pure state

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{streamId} =
  case msg of
    Tick -> CastNoReply <$> handleTick state

    InitStreamRelays -> CastNoReply <$> initStreamRelay state

    MaybeStop ref -> maybeStop ref state

    IntraPoPBus (IngestAggregatorExited stoppingStreamId serverAddress)
      | stoppingStreamId == streamId -> pure $ CastStop state
      | otherwise -> pure $ CastNoReply state


handleTick :: State -> Effect State
handleTick state@{streamId} = do
  _ <- IntraPoP.announceEgestIsAvailable streamId
  pure state

maybeStop :: Ref -> State -> Effect (CastResult State)
maybeStop ref state@{streamId
                    , clientCount
                    , stopRef}
  | (clientCount == 0) && (Just ref == stopRef) = do
    _ <- logInfo "Egest stopping" {streamId: streamId}
    _ <- IntraPoP.announceEgestStopped streamId
    pure $ CastStop state

  | otherwise = pure $ CastNoReply state

initStreamRelay :: State -> Effect State
initStreamRelay state@{relayCreationRetry, streamId, aggregatorPoP, thisServer} = do
  relayResp <- findOrStartRelayForStream state
  case spy "initStreamRelay - relayResp" relayResp of
    Left _ ->  do
      _ <- Timer.sendAfter (serverName streamId) (unwrap relayCreationRetry) InitStreamRelays
      pure state
    Right local@(Local _) -> do
      let _ = spy "initStreamRelay - register" {}
      _ <- StreamRelayInstance.registerEgest relayRegistrationPayload
      pure state{relay = Just $ toRelayServer <$> (spy "initStreamRelay - local" local)}

    Right remote@(Remote remoteServer) -> do
      let
        url = makeUrl remoteServer RelayE
      _ <- void <$> crashIfLeft =<< SpudGun.postJson url relayRegistrationPayload
      pure state{relay = Just $ toRelayServer  <$> remote}
  where
    relayRegistrationPayload  :: RegisterEgestPayload
    relayRegistrationPayload =
      {streamId, deliverTo: state.thisServer}




--------------------------------------------------------------------------------
-- Do we have a relay in this pop - yes -> use it
-- Does the stream have an origin (aggregator) - if so build a streamRelay chain to that origin
-- otherwise 404
--------------------------------------------------------------------------------
findOrStartRelayForStream :: State -> Effect (ResourceResp Server)
findOrStartRelayForStream state@{streamId, thisServer} = do
  mRelay <- IntraPoP.whereIsStreamRelay streamId
  maybe' (\_ -> launchLocalOrRemote state) (pure <<< Right) mRelay


launchLocalOrRemote :: State -> Effect (ResourceResp Server)
launchLocalOrRemote state@{streamId, aggregatorPoP, thisServer} =
  launchLocalOrRemoteGeneric filterForLoad launchLocal launchRemote
  where
    payload = {streamId, aggregatorPoP} :: CreateRelayPayload
    launchLocal _ = do
      _ <- StreamRelayInstanceSup.startRelay payload
      pure unit
    launchRemote idleServer =
      let
        url = makeUrl idleServer RelayE
      in void <$> crashIfLeft =<< SpudGun.postJson url payload


toRelayServer :: forall a b. Newtype a b => Newtype RelayServer b => a -> RelayServer
toRelayServer = unwrap >>> wrap




loadThresholdToCreateRelay :: Load
loadThresholdToCreateRelay = wrap 50.0

filterForLoad :: ServerLoad -> Boolean
filterForLoad (ServerLoad sl) = sl.load < loadThresholdToCreateRelay


 -- IngestAggregator - .... - .... - ... - Egest
 -- TheirPoP ............................. OurPoP

 -- 1. compute pop route ->
 --     1. [ ThePoP ]
 --     2.
 --        [ TheirPoP, IntermediatePoP, OurPoP ]
 --        [ TheirPoP, OtherIntermediatePoP1, OtherIntermediaPoP2, OurPoP ]

 -- 2. Create the relay for our pop - passing it the entirety of both chains
 --    detail: what does that means? is it just an HTTP request to some endpoint? yes

 -- that is everything


  -- from our relay's point of view, it needs to:
  -- if we're in the same pop as the aggregator - source from the ingest aggregator directly
  -- if we're in a different pop
  --   for each of the chains, pick the next pop in the next chain, and ask it to relay to us passing in the chain information relevant to it
  --      detail: to what server in the next pop do we talk?

  -- additional thoughts on load and stuff:
  -- If aggregator is in this pop pick server for the relay
  --   Needs capacity
  --   Prefer with most capacity (if any have enough) and create a relay and an edge on it
  -- If we are on the same server as the IngestAggregator and we have capacity, then create a single relay here
  -- same pop -> 1) If server with aggregator has cap


-- createRelayInThisPoP :: StreamId -> PoPName -> List ViaPoPs -> Server -> Effect (Either FailureReason Server)
-- createRelayInThisPoP streamId thisPoPName routes ingestAggregator = do
--   maybeCandidateRelayServer <- IntraPoP.getIdleServer (const true)

--   case (spy "maybeCandidateRelayServer" maybeCandidateRelayServer) of
--     Just candidateRelayServer ->
--       let
--         url = makeUrl candidateRelayServer RelayE

--         request =
--           { streamId,
--             aggregator: ingestAggregator
--           , routes: List.toUnfoldable <$> List.toUnfoldable routes
--           } :: CreateRelayPayload
--       in
--       do
--         -- TODO / thoughts - do we wait for the entire relay chain to exist before returning?
--         -- what if there isn't enough resource on an intermediate PoP?
--         -- Single relay that goes direct?
--         _restResult <- SpudGun.postJson url request
--         pure $ Right $ serverLoadToServer candidateRelayServer

--     Nothing ->
--       pure $ Left NoResource


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = Agent.Egest # show # atom # singleton

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

-- logWarning :: forall a. Logger a
-- logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
