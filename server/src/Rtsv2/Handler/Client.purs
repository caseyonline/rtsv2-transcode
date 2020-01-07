module Rtsv2.Handler.Client
       (
         --clientStart
         clientStop
       ) where

import Prelude

import Data.Either (Either)
import Data.Foldable (minimum)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.Rest (moved, notMoved)
import Erl.Cowboy.Req (binding, header, path, setHeader)
import Erl.Data.List (List, head, nil, null, (:))
import Erl.Data.Tuple (tuple2)
import Logger as Logger
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Audit as Audit
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Utils (member)
import Shared.Stream (StreamId(..))
import Shared.Types (ServerAddress(..), ServerLoad(..))
import Shared.Utils (lazyCrashIfMissing)
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

data ClientStartState =
  NoEdgeRole
  | StreamNotAvailable
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
-- findEgestForStream :: StreamId -> Effect ServerSelectionResponse
-- findEgestForStream streamId = do
--   thisNode <- PoPDefinition.thisNode
--   servers <- IntraPoP.whereIsEgest streamId

--   let serversWithCapacity  =
--         filter (\ServerLoad serverAddress load ->
--                  load < 76.323341
--                ) servers
--   if any ((==) thisNode <<< extractAddress) servers
--   then pure Local
--   else
--    case pickInstance serversWithCapacity of
--      Just server -> pure Remote server
--      Nothing ->
--        case findRelayForStream streamId of

--        pure $ Local
--   where extractAddress (ServerLoad serverAddress _) = serverAddress
--         pickInstance = extractAddress <$> head


-- --------------------------------------------------------------------------------
-- -- Do we have a relay in this pop - yes -> use it
-- -- Does the stream have an origin (aggregator) - if so build a streamRelay chain to that origin
-- -- otherwise 404
-- --------------------------------------------------------------------------------
-- findRelayForStream :: StreamId -> Effect (Either FailureType ServerAddress)
-- findRelayForStream streamId = do
--   mRelay <- IntraPoP.whereIsStreamRelay stream
--   case mRelay of
--     Just relay -> pure $ Right relay
--     Nothig -> do
--       mAggregator <- IntraPoP.whereIsIngestAggregator streamId
--       case mAggregator of
--         Nothing -> Left NotFound
--         Just sourceServer -> do
--           -- Find a server with capacity for a new relay
--           leastLoaded = IntraPoP.w
--           createRelayChain sourceServer streamId

-- createRelayChain :: ServerAddress -> StreamId -> Effect (Either FailureType ServerAddress)
-- createRelayChain sourceServer streamId = do
--   pure $ Right sourceServer



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
                                Rest.result ((tuple2 "text/plain" removeClient) : nil) req state)
  # Rest.yeeha

  where
    removeClient req state@{streamId} = do
      _ <- Audit.clientStop streamId
      _ <- EgestInstance.removeClient streamId
      Rest.result "" req state
