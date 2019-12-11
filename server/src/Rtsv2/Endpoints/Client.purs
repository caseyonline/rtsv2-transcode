module Rtsv2.Endpoints.Client
       (
         clientStart
       , clientStop
       ) where

import Prelude

import Data.Foldable (minimum)
import Data.Maybe (Maybe(..), fromMaybe')
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.Rest (moved, notMoved)
import Erl.Cowboy.Req (binding, path, setHeader)
import Erl.Data.List (List, nil, null, (:))
import Erl.Data.Tuple (tuple2)
import Logger as Logger
import Rtsv2.Audit as Audit
import Rtsv2.EdgeAgentSup as EdgeAgentSup
import Rtsv2.EdgeAgent as EdgeAgent
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.PoPDefinition (ServerAddress)
import Rtsv2.PoPDefinition as PoPDefintion
import Rtsv2.Utils (member)
import Shared.Stream (StreamId(..))
import Shared.Utils (lazyCrashIfMissing)
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

type ClientStartState = { streamId :: StreamId
                   , isIngestAvailable :: Boolean
                   , currentNodeHasEdge :: Boolean
                   , thisNode :: ServerAddress
                   , currentEdgeLocations :: List ServerAddress
                 }
clientStart :: StetsonHandler ClientStartState
clientStart =
  Rest.handler (\req ->
                 let streamId = StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                 in
                  do
                    thisNode <- PoPDefintion.thisNode
                    Rest.initResult req { streamId : streamId
                                        , isIngestAvailable : false
                                        , currentNodeHasEdge : false
                                        , thisNode : thisNode
                                        , currentEdgeLocations : nil
                                        }
               )
  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- EdgeAgentSup.isAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{streamId, thisNode} -> do
                            isIngestAvailable <- IntraPoPAgent.isStreamIngestAvailable streamId
                            currentEdgeLocations <- IntraPoPAgent.whereIsEdge streamId
                            let
                              currentNodeHasEdge = member thisNode currentEdgeLocations
                              exists = isIngestAvailable && (currentNodeHasEdge || (null currentEdgeLocations))
                            Rest.result exists req state {isIngestAvailable = isIngestAvailable
                                                         , currentNodeHasEdge = currentNodeHasEdge
                                                         , currentEdgeLocations = currentEdgeLocations}
                        )

  # Rest.previouslyExisted (\req state@{isIngestAvailable} -> do
                               _ <- Logger.info "PreviouslyExisted" {misc: {isIngestAvailable}}
                               Rest.result isIngestAvailable req state)

  # Rest.movedTemporarily (\req state@{currentEdgeLocations} -> do
                            _ <- Logger.info "MovedTemp" {misc: {currentEdgeLocations}}
                            let
                              -- todo - ick - need functions to build URLs
                              redirect server = "http://" <> server <> ":3000" <> (path req)
                            case pickBest currentEdgeLocations of
                              Nothing -> Rest.result notMoved req state
                              Just server -> Rest.result (moved (redirect server)) req state
                          )

  # Rest.contentTypesProvided (\req state ->
                                Rest.result ((tuple2 "text/plain" addOrStartEdge) : nil) req state)
  # Rest.yeeha

  where
    addOrStartEdge req state@{thisNode, streamId} = do
      _ <- Audit.clientStart streamId
      _ <- EdgeAgentSup.maybeStartAndAddClient streamId
      let
        req2 = setHeader "X-ServedBy" thisNode req
      Rest.result "" req2 state
    pickBest = minimum

type ClientStopState = { streamId :: StreamId
                       }
clientStop :: StetsonHandler ClientStopState
clientStop =
  Rest.handler (\req ->
                 let streamId = StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                 in
                  do
                    thisNode <- PoPDefintion.thisNode
                    Rest.initResult req { streamId : streamId
                                        }
               )
  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- EdgeAgentSup.isAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{streamId} -> do
                            isActive <- EdgeAgent.isActive streamId
                            Rest.result isActive req state
                        )

  # Rest.contentTypesProvided (\req state ->
                                Rest.result ((tuple2 "text/plain" removeClient) : nil) req state)
  # Rest.yeeha

  where
    removeClient req state@{streamId} = do
      _ <- Audit.clientStop streamId
      _ <- EdgeAgent.removeClient streamId
      Rest.result "" req state
