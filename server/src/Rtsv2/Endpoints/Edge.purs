module Rtsv2.Endpoints.Edge
       (
         edgeStart
       ) where

import Prelude

import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Set (Set, findMin, isEmpty, member)
import Data.Set as Set
import Debug.Trace (spy)
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.Rest (moved, notMoved)
import Erl.Cowboy.Req (binding, path, setHeader)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.EdgeAgentSup (startEdge)
import Rtsv2.EdgeAgentSup as EdgeAgentSup
import Rtsv2.Endpoints.MimeType as MimeType
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.PoPDefinition (ServerAddress)
import Rtsv2.PoPDefinition as PoPDefintion
import Rtsv2.TransPoPAgent as Logger
import Rtsv2.TransPoPAgent as TransPoPAgent
import Shared.Stream (StreamId(..))
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as JSON
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

type EdgeState = { streamId :: StreamId
                 , isAgentAvailable :: Boolean
                 , isIngestAvailable :: Boolean
                 , currentNodeHasEdge :: Boolean
                 , thisNode :: ServerAddress
                 , currentEdgeLocations :: Set ServerAddress
                 }
edgeStart :: StetsonHandler EdgeState
edgeStart =
  Rest.handler (\req ->
                 let streamId = StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                 in
                  do
                    -- TODO - more RESTy please - but we need isConflict to get the flow correct
                    thisNode <- PoPDefintion.thisNode
                    isAgentAvailable <- EdgeAgentSup.isAvailable
                    isIngestAvailable <- IntraPoPAgent.isStreamIngestAvailable streamId
                    currentEdgeLocations <- fromMaybe Set.empty <$> IntraPoPAgent.whereIsEdge streamId
                    Rest.initResult req { streamId : streamId
                                        , thisNode
                                        , isAgentAvailable
                                        , isIngestAvailable
                                        , currentEdgeLocations
                                        , currentNodeHasEdge : member thisNode $ currentEdgeLocations
                                        }
               )
  # Rest.serviceAvailable (\req state@{isAgentAvailable} -> Rest.result isAgentAvailable req state)
  # Rest.resourceExists (\req state@{isIngestAvailable, currentNodeHasEdge, currentEdgeLocations} ->
                          let
                            exists = isIngestAvailable && (currentNodeHasEdge || (isEmpty currentEdgeLocations))
                          in
                           Rest.result exists req state
                        )
  # Rest.previouslyExisted (\req state@{isIngestAvailable} -> Rest.result isIngestAvailable req state)
  # Rest.movedTemporarily (\req state@{currentEdgeLocations} ->
                            let
                              -- todo - ick - need functions to build URLs
                              redirect server = "http://" <> server <> ":3000" <> (path req)
                            in
                            case spy "min" (findMin currentEdgeLocations) of
                              Nothing -> Rest.result notMoved req state
                              Just server -> Rest.result (moved (redirect server)) req state
                          )
  # Rest.contentTypesProvided (\req state -> do
                                  _ <- Logger.logInfo "contentTypesProvided" {state: state}
                                  Rest.result
                                      ((tuple2 "text/plain" addOrStartEdge) : nil)
                                      req state)
  # Rest.yeeha

  where
    addOrStartEdge req state@{currentNodeHasEdge: false} = do
      _ <- startEdge state.streamId
      Rest.result "" req state
    addOrStartEdge req state@{currentNodeHasEdge: true,
                              thisNode} = do
      -- TODO - this could fail, in which case we should return conflict
      _ <- startEdge state.streamId
      let
        req2 = setHeader "X-ServedBy" thisNode req
      Rest.result "" req2 state
