module Rtsv2.Handler.Client
       ( clientStart
       , clientStop
       , ServerSelectionResponse
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Req (binding)
import Erl.Data.List (List, singleton, (:))
import Erl.Data.List as List
import Erl.Data.Tuple (tuple2)
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
import Stetson (HttpMethod(..), StetsonHandler)
import Stetson.Rest as Rest

data ClientStartState
  = StreamNotAvailable
  | Foo  { streamId :: StreamId
         , isIngestAvailable :: Boolean
         , currentNodeHasEdge :: Boolean
         , thisNode :: ServerAddress
         , currentEdgeLocations :: List ServerAddress
         }

data ServerSelectionResponse
  = NotFound
  | NoResource
  | Local
  | Remote ServerAddress


data FailureType
  = FTNotFound
  | FTNoResource


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
findEgestForStream :: StreamId -> Effect ServerSelectionResponse
findEgestForStream streamId = do
  thisNode <- PoPDefinition.thisNode
  servers <- IntraPoP.whereIsEgest streamId

  let serversWithCapacity  =
        List.filter (\(ServerLoad serverAddress load) ->
                      load < (wrap 76.323341)
                    ) servers
  if any ((==) thisNode <<< extractAddress) servers
  then pure Local
  else
   case pickInstance serversWithCapacity of
     Just server ->
       pure $ Remote server

     Nothing ->
       do
         relayForStream <- findRelayForStream streamId

         pure $
           case relayForStream of
             Left _ ->
               NoResource

             Right serverAddress
               | serverAddress == thisNode -> Local
               | otherwise -> Remote serverAddress

  where
    extractAddress (ServerLoad locatedServer _) = locatedServerAddress locatedServer
    pickInstance = map extractAddress <<< List.head

--------------------------------------------------------------------------------
-- Do we have a relay in this pop - yes -> use it
-- Does the stream have an origin (aggregator) - if so build a streamRelay chain to that origin
-- otherwise 404
--------------------------------------------------------------------------------
findRelayForStream :: StreamId -> Effect (Either FailureType ServerAddress)
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
            pure $ Left FTNotFound

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
createRelayChain :: LocatedServer -> StreamId -> Effect (Either FailureType ServerAddress)
createRelayChain ingestAggregatorServer@(LocatedServer address (ServerLocation pop _region)) streamId = do

  { name: thisPoPName } <- PoPDefinition.thisPoP

  upstreamPoPs <-
    if pop == thisPoPName then
      pure $ List.singleton mempty
    else
      TransPoP.routesTo pop

  createRelayInThisPoP streamId pop upstreamPoPs ingestAggregatorServer


createRelayInThisPoP :: StreamId -> PoPName -> List ViaPoPs -> LocatedServer -> Effect (Either FailureType ServerAddress)
createRelayInThisPoP streamId thisPoPName routes ingestAggregator@(LocatedServer address _location) = do
  maybeCandidateRelayServer <- IntraPoP.getIdleServer (const true)

  case maybeCandidateRelayServer of
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
        _restResult <- SpudGun.post url (Json.writeJSON request)
        pure $ Right $ locatedServerAddress candidateRelayServer

    Nothing ->
      pure $ Left FTNoResource


toHost :: LocatedServer -> String
toHost =
  unwrap <<< locatedServerAddress

clientStart :: StetsonHandler ServerSelectionResponse
clientStart =
  Rest.handler init
  # Rest.allowedMethods (Rest.result (PUT : mempty))
  # Rest.contentTypesAccepted (\req state -> Rest.result (singleton $ MimeType.json acceptAny) req state)
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.json provideEmpty) req state)
  # Rest.yeeha

  where
    init req =
      let
        streamId = spy "clientStartInit" $ StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
      in
        do
          thisNode <- PoPDefinition.thisNode
          egestResp <- spy "clientStartInit" $ findEgestForStream streamId
          Rest.initResult req egestResp

    acceptAny req state = Rest.result true req state

    provideEmpty req state = Rest.result "" req state


-- clientStart :: StetsonHandler ClientStartState
-- clientStart =
--   Rest.handler
--   (\req ->
--     let streamId = StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
--     in
--      do
--        eEgest <- findEgestForStream
--        thisNode <- PoPDefinition.thisNode
--        isAgentAvailable <- EdgeInstanceSup.isAvailable
--        Rest.initResult req $
--          if isAgentAvailable then
--            case IntraPoP.whereIsStreamAggregator streamId of
--              Just serverAddress ->

--                | serverAddress == thisNode ->
--              Nothing

--            isIngestAvailable <-
--            Foo { streamId : streamId
--                , isIngestAvailable : false
--                , currentNodeHasEdge : false
--                , thisNode : thisNode
--                , currentEdgeLocations : nil
--                }
--          else
--            NoEdgeRole
--   )
--   # Rest.serviceAvailable (\req state -> do
--                               isAgentAvailable <- EdgeInstanceSup.isAvailable
--                               Rest.result isAgentAvailable req state)

--   # Rest.resourceExists (\req state@{streamId, thisNode} -> do
--                             isIngestAvailable <- IntraPoP.isStreamIngestAvailable streamId
--                             currentEdgeLocations <- IntraPoP.whereIsEdge streamId
--                             let
--                               currentNodeHasEdge = member thisNode currentEdgeLocations
--                               exists = isIngestAvailable && (currentNodeHasEdge || (null currentEdgeLocations))
--                             Rest.result exists req state {isIngestAvailable = isIngestAvailable
--                                                          , currentNodeHasEdge = currentNodeHasEdge
--                                                          , currentEdgeLocations = currentEdgeLocations}
--                         )

--   # Rest.previouslyExisted (\req state@{isIngestAvailable} -> do
--                                _ <- Logger.info "PreviouslyExisted" {misc: {isIngestAvailable}}
--                                Rest.result isIngestAvailable req state)

--   # Rest.movedTemporarily (\req state@{currentEdgeLocations} -> do
--                             _ <- Logger.info "MovedTemp" {misc: {currentEdgeLocations}}
--                             let
--                               -- todo - ick - need functions to build URLs
--                               redirect (ServerAddress server) = "http://" <> server <> ":3000" <> (path req)
--                             case pickBest currentEdgeLocations of
--                               Nothing -> Rest.result notMoved req state
--                               Just server -> Rest.result (moved (redirect server)) req state
--                           )

--   # Rest.contentTypesProvided (\req state ->
--                                 Rest.result ((tuple2 "text/plain" addOrStartEdge) : nil) req state)
--   # Rest.yeeha

--   where
--     addOrStartEdge req state@{thisNode, streamId} = do
--       _ <- Audit.clientStart streamId
--       _ <- EdgeInstanceSup.maybeStartAndAddClient streamId
--       let
--         req2 = setHeader "X-ServedBy" (unwrap thisNode) req
--       Rest.result "" req2 state
--     pickBest :: List ServerAddress -> Maybe ServerAddress
--     pickBest = minimum

type ClientStopState = { streamId :: StreamId
                       }
clientStop :: StetsonHandler ClientStopState
clientStop =
  Rest.handler (\req ->
                 let streamId = StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                 in
                  do
                    thisNode <- PoPDefinition.thisNode
                    Rest.initResult req { streamId : streamId
                                        }
               )
  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- EgestInstanceSup.isAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{streamId} -> do
                            isActive <- EgestInstance.isActive streamId
                            Rest.result isActive req state
                        )

  # Rest.contentTypesProvided (\req state ->
                                Rest.result ((tuple2 "text/plain" removeClient) : List.nil) req state)
  # Rest.yeeha

  where
    removeClient req state@{streamId} = do
      _ <- Audit.clientStop streamId
      _ <- EgestInstance.removeClient streamId
      Rest.result "" req state

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
