module Rtsv2.Handler.Client
       ( clientStart
       , clientStop
       , State
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Handlers.Rest (MovedResult, moved, notMoved)
import Erl.Cowboy.Req (Req, StatusCode(..), binding, replyWithoutBody, setHeader)
import Erl.Data.List (List, singleton, (:))
import Erl.Data.List as List
import Erl.Data.Map as Map
import Logger (Logger, spy)
import Logger as Logger
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.TransPoP (ViaPoPs)
import Rtsv2.Agents.TransPoP as TransPoP
import Rtsv2.Audit as Audit
import Rtsv2.Handler.MimeType as MimeType
import Rtsv2.Handler.Relay (CreateRelayPayload)
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..))
import Rtsv2.Router.Endpoint as RoutingEndpoint
import Rtsv2.Router.Parser as Routing
import Shared.Stream (StreamId(..))
import Shared.Types (LocatedServer(..), PoPName, ServerAddress, ServerLoad(..), ServerLocation(..), locatedServerAddress)
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as Json
import SpudGun as SpudGun
import Stetson (HttpMethod(..), RestResult, StetsonHandler)
import Stetson.Rest as Rest

data ClientStartState
  = StreamNotAvailable
  | Foo  { streamId :: StreamId
         , isIngestAvailable :: Boolean
         , currentNodeHasEdge :: Boolean
         , thisNode :: ServerAddress
         , currentEdgeLocations :: List ServerAddress
         }

data EgestLocation
  = Local
  | Remote ServerAddress

data FailureReason
  = NotFound
  | NoResource


newtype State = State { streamId :: StreamId
                      , egestResp :: (Either FailureReason EgestLocation)
                      }
--derive instance newtypeState :: Newtype State _

--------------------------------------------------------------------------------
-- Do we have any EgestInstances in this pop that have capacity for another client?
-- Yes -> If this node is one of them -> Use it (TODO - maybe load concentrate?)
--                          Otherwise -> Redirect the client to one of those servers
--  No -> Is there a stream relay already running in the pop?
--          Yes -> Does anywhere have capacity to start a egest instance
--                   Yes -> Redirect to it
--                    No -> Fail request (out of resources)
--          No -> Try to find or create a relay for this stream and also put an egest handler on the same node
-- otherwise 404
--------------------------------------------------------------------------------
findEgestForStream :: ServerAddress -> StreamId -> Effect (Either FailureReason EgestLocation)
findEgestForStream thisNode streamId = do
  servers <- IntraPoP.whereIsEgest streamId

  let serversWithCapacity  =
        List.filter (\(ServerLoad serverAddress load) ->
                      load < (wrap 76.323341)
                    ) $ spy "swc" $ servers
  if any ((==) thisNode <<< extractAddress) servers
  then
    do
      _ <- EgestInstanceSup.maybeStartAndAddClient streamId
      pure $ Right Local
  else
   case pickInstance serversWithCapacity of
     Just server ->
       pure $ Right $ Remote (spy "remote" server)

     Nothing ->
       do
         relayForStream <- findRelayForStream streamId

         case relayForStream of
           Left e ->
             pure $ Left e

           Right relayAddress ->
             do
               _ <- EgestInstanceSup.maybeStartAndAddClient streamId
               pure $ Right Local


  where
    extractAddress (ServerLoad locatedServer _) = locatedServerAddress locatedServer
    pickInstance = map extractAddress <<< List.head

--------------------------------------------------------------------------------
-- Do we have a relay in this pop - yes -> use it
-- Does the stream have an origin (aggregator) - if so build a streamRelay chain to that origin
-- otherwise 404
--------------------------------------------------------------------------------
findRelayForStream :: StreamId -> Effect (Either FailureReason ServerAddress)
findRelayForStream streamId = do
  mRelay <- IntraPoP.whereIsStreamRelay streamId

  case mRelay of
    Just relay ->
      pure $ Right relay

    Nothing ->
      do
        mAggregator <- IntraPoP.whereIsIngestAggregator streamId

        case mAggregator of
          Nothing ->
            pure $ Left NotFound

          Just sourceServer -> do
            -- Find a server with capacity for a new relay
            --leastLoaded <- IntraPoP.w
            createRelayChain sourceServer streamId


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
createRelayChain :: LocatedServer -> StreamId -> Effect (Either FailureReason ServerAddress)
createRelayChain ingestAggregatorServer@(LocatedServer address (ServerLocation pop _region)) streamId = do

  { name: thisPoPName } <- PoPDefinition.thisPoP

  upstreamPoPs <-
    if pop == thisPoPName then
      pure $ List.singleton mempty
    else
      TransPoP.routesTo pop

  createRelayInThisPoP streamId pop upstreamPoPs ingestAggregatorServer


createRelayInThisPoP :: StreamId -> PoPName -> List ViaPoPs -> LocatedServer -> Effect (Either FailureReason ServerAddress)
createRelayInThisPoP streamId thisPoPName routes ingestAggregator@(LocatedServer address _location) = do
  maybeCandidateRelayServer <- IntraPoP.getIdleServer (const true)

  case (spy "maybeCandidateRelayServer" maybeCandidateRelayServer) of
    Just candidateRelayServer ->
      let
        path = Routing.printUrl RoutingEndpoint.endpoint (RelayE streamId)
        url = spy "url" $ "http://" <> toHost candidateRelayServer <> ":3000" <> path

        request =
          { streamSource: address
          , routes: List.toUnfoldable $ map List.toUnfoldable routes
          } :: CreateRelayPayload
      in
      do
        -- TODO / thoughts - do we wait for the entire relay chain to exist before returning?
        -- what if there isn't enough resource on an intermediate PoP?
        -- Single relay that goes direct?
        _restResult <- SpudGun.postJson (wrap url) $ request
        pure $ Right $ locatedServerAddress candidateRelayServer

    Nothing ->
      pure $ Left NoResource


toHost :: LocatedServer -> String
toHost =
  unwrap <<< locatedServerAddress

clientStart :: StetsonHandler State
clientStart =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptAny) req state)
  # Rest.resourceExists resourceExists
  # Rest.previouslyExisted previouslyExisted
  # Rest.movedTemporarily movedTemporarily
  # Rest.yeeha

  where
    init req =
      let
        streamId = spy "clientStartInit" $ StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
      in
        do
          thisNode <- PoPDefinition.thisNode
          egestResp <- findEgestForStream thisNode streamId
          let
            req2 = setHeader "x-servedby" (unwrap thisNode) req
            _ = spy "egestResp" egestResp
          Rest.initResult req2 $ State { streamId, egestResp }

    acceptAny req state = Rest.result true req state

    resourceExists req state@(State {egestResp}) =
      case egestResp of
        Left NotFound ->
          do
            newReq <- replyWithoutBody (StatusCode 404) Map.empty req
            Rest.stop newReq state
        Left NoResource ->
          -- todo
          Rest.stop req state
        Right Local ->
          Rest.result true req state
        Right (Remote _) ->
          Rest.result false req state

    previouslyExisted req state@(State {egestResp}) =
      let resp = case egestResp of
            Right (Remote _) -> true
            _ -> false
      in
       Rest.result resp req state

    movedTemporarily :: Req -> State -> Effect (RestResult MovedResult State)
    movedTemporarily req state@(State {streamId, egestResp}) =
      case spy "moved" egestResp of
        Right (Remote addr) ->
          let
            path = Routing.printUrl RoutingEndpoint.endpoint (ClientStartE "canary" streamId)
            url = spy "url" $ "http://" <> unwrap addr <> ":3000" <> path
          in
            Rest.result (moved $ url) req state
        _ ->
          Rest.result notMoved req state





type ClientStopState = { streamId :: StreamId
                       }
clientStop :: StetsonHandler ClientStopState
clientStop =

  Rest.handler init
  # Rest.allowedMethods (Rest.result (POST : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json removeClient) req state)
  # Rest.resourceExists resourceExists
  # Rest.yeeha

  where
    init req =
      let streamId = StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
      in
       do
         thisNode <- PoPDefinition.thisNode
         Rest.initResult req { streamId : spy "init" streamId
                             }

    acceptAny req state = Rest.result true req  $ spy "accept" state

    serviceAvailable req state = do
      isAgentAvailable <- EgestInstanceSup.isAvailable
      Rest.result isAgentAvailable req  $ spy "savail" state


    resourceExists req state@{streamId} = do
      isActive <- EgestInstance.isActive $ spy "exists" streamId
      Rest.result isActive req state

    removeClient req state@{streamId} =
      let _ = spy "remove" streamId
      in do
      _ <- Audit.clientStop streamId
      _ <- EgestInstance.removeClient $ spy "remove" streamId
      Rest.result true req state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> ("Client" :  "Instance" : List.nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
