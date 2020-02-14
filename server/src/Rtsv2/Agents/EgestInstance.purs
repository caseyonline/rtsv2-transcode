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
import Erl.Utils (Ref, makeRef)
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
import Shared.Stream (AggregatorKey(..), EgestKey(..), RelayKey(..), StreamId, StreamRole(..))
import Shared.Types (Milliseconds, EgestServer, Load, PoPName, RelayServer, Server, ServerLoad(..))
import Shared.Types.Agent.State as PublicState
import SpudGun as SpudGun

type CreateEgestPayload
  = { streamId :: StreamId
    , aggregatorPoP :: PoPName
    }

type State
  = { egestKey :: EgestKey
    , aggregatorPoP :: PoPName
    , thisServer :: EgestServer
    , relay :: Maybe (LocalOrRemote RelayServer)
    , clientCount :: Int
    , lingerTime :: Milliseconds
    , relayCreationRetry :: Milliseconds
    , stopRef :: Maybe Ref
    }

payloadToEgestKey :: CreateEgestPayload -> EgestKey
payloadToEgestKey payload = EgestKey payload.streamId

data Msg = Tick
         | InitStreamRelays
         | MaybeStop Ref
         | IntraPoPBus IntraPoP.IntraPoPBusMessage

serverName :: EgestKey -> ServerName State Msg
serverName egestKey = Names.egestInstanceName egestKey

isActive :: EgestKey -> Effect Boolean
isActive egestKey = Pinto.isRegistered (serverName egestKey)

startLink :: CreateEgestPayload -> Effect StartLinkResult
startLink payload = Gen.startLink (serverName $ payloadToEgestKey payload) (init payload) handleInfo

addClient :: EgestKey -> Effect RegistrationResp
addClient egestKey = do
  Gen.doCall (serverName egestKey) doAddClient

doAddClient :: State -> Effect (CallResult RegistrationResp State)
doAddClient state@{clientCount} = do
  logInfo "Add client" {newCount: clientCount + 1}
  pure $ CallReply (Right unit) state{ clientCount = clientCount + 1
                                     , stopRef = Nothing
                                     }

removeClient :: EgestKey -> Effect Unit
removeClient egestKey = Gen.doCall (serverName egestKey) doRemoveClient

doRemoveClient :: State -> Effect (CallResult Unit State)
doRemoveClient state@{clientCount: 0} = do
  logInfo "Remove client - already zero" {}
  pure $ CallReply unit state
doRemoveClient state@{clientCount: 1, lingerTime, egestKey} = do
  ref <- makeRef
  logInfo "Last client gone, start stop timer" {}
  _ <- Timer.sendAfter (serverName egestKey) (unwrap lingerTime) (MaybeStop ref)
  pure $ CallReply unit state{ clientCount = 0
                             , stopRef = Just ref
                             }

doRemoveClient state@{clientCount} = do
  logInfo "Remove client" { newCount: clientCount - 1 }
  pure $ CallReply unit state{ clientCount = clientCount - 1 }

currentStats :: EgestKey -> Effect PublicState.Egest
currentStats egestKey =
  Gen.call (serverName egestKey) \state@{clientCount} ->
    CallReply {clientCount} state


toEgestServer :: Server -> EgestServer
toEgestServer = unwrap >>> wrap

init :: CreateEgestPayload -> Effect State
init payload@{streamId, aggregatorPoP} = do
  let
    egestKey = payloadToEgestKey payload
    relayKey = RelayKey streamId Primary
  logInfo "Egest starting" {payload}
  _ <- Bus.subscribe (serverName egestKey) IntraPoP.bus IntraPoPBus
  {egestAvailableAnnounceMs, lingerTimeMs, relayCreationRetryMs} <- Config.egestAgentConfig

  thisServer <- PoPDefinition.getThisServer
  _ <- IntraPoP.announceLocalEgestIsAvailable egestKey
  _ <- Timer.sendEvery (serverName egestKey) egestAvailableAnnounceMs Tick
  _ <- Timer.sendAfter (serverName egestKey) 0 InitStreamRelays

  maybeRelay <- IntraPoP.whereIsStreamRelay relayKey
  let
    state ={ egestKey
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
      _ <- StreamRelayInstanceSup.startRelay {streamId, streamRole: Primary, aggregatorPoP}
      pure state

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{egestKey: EgestKey ourStreamId} =
  case msg of
    Tick -> CastNoReply <$> handleTick state

    InitStreamRelays -> CastNoReply <$> initStreamRelay state

    MaybeStop ref -> maybeStop ref state

    IntraPoPBus (IngestAggregatorExited (AggregatorKey streamId streamRole) serverAddress)
     -- TODO - PRIMARY BACKUP
      | streamId == ourStreamId -> doStop state
      | otherwise -> pure $ CastNoReply state


handleTick :: State -> Effect State
handleTick state@{egestKey} = do
  _ <- IntraPoP.announceLocalEgestIsAvailable egestKey
  pure state

maybeStop :: Ref -> State -> Effect (CastResult State)
maybeStop ref state@{ clientCount
                    , stopRef
                    }
  | (clientCount == 0) && (Just ref == stopRef) = doStop state
  | otherwise = pure $ CastNoReply state

doStop :: State -> Effect (CastResult State)
doStop state@{egestKey} = do
  logInfo "Egest stopping" {egestKey}
  IntraPoP.announceLocalEgestStopped egestKey
  pure $ CastStop state


initStreamRelay :: State -> Effect State
initStreamRelay state@{relayCreationRetry, egestKey: egestKey@(EgestKey streamId), aggregatorPoP, thisServer} = do
  relayResp <- findOrStartRelayForStream state
  case spy "initStreamRelay - relayResp" relayResp of
    Left _ ->  do
      _ <- Timer.sendAfter (serverName egestKey) (unwrap relayCreationRetry) InitStreamRelays
      pure state
    Right local@(Local _) -> do
      let _ = spy "initStreamRelay - register" {}
      _ <- StreamRelayInstance.registerEgest relayRegistrationPayload
      pure state{relay = Just $ toRelayServer <$> (spy "initStreamRelay - local" local)}

    Right remote@(Remote remoteServer) -> do
      let
        url = makeUrl remoteServer RelayRegisterEgestE
      _ <- void <$> crashIfLeft =<< SpudGun.postJson url relayRegistrationPayload
      pure state{relay = Just $ toRelayServer  <$> remote}
  where
    relayRegistrationPayload  :: RegisterEgestPayload
    relayRegistrationPayload =
      {streamId, streamRole: Primary, deliverTo: state.thisServer}




--------------------------------------------------------------------------------
-- Do we have a relay in this pop - yes -> use it
-- Does the stream have an origin (aggregator) - if so build a streamRelay chain to that origin
-- otherwise 404
--------------------------------------------------------------------------------
findOrStartRelayForStream :: State -> Effect (ResourceResp Server)
findOrStartRelayForStream state@{egestKey: EgestKey streamId, thisServer} = do
  mRelay <- IntraPoP.whereIsStreamRelay $ RelayKey streamId Primary
  maybe' (\_ -> launchLocalOrRemote state) (pure <<< Right) mRelay


launchLocalOrRemote :: State -> Effect (ResourceResp Server)
launchLocalOrRemote state@{egestKey: egestKey@(EgestKey streamId), aggregatorPoP, thisServer} =
  launchLocalOrRemoteGeneric filterForLoad launchLocal launchRemote
  where
    payload = {streamId, streamRole: Primary, aggregatorPoP} :: CreateRelayPayload
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


-- createRelayInThisPoP :: EgestKey -> PoPName -> List ViaPoPs -> Server -> Effect (Either FailureReason Server)
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
