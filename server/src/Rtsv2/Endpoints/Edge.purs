module Rtsv2.Endpoints.Edge
       (
         clientCount
       ) where

import Prelude

import Data.Maybe (fromMaybe, fromMaybe')
import Data.Set (member)
import Data.Set as Set
import Debug.Trace (spy)
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.Rest (moved, notMoved)
import Erl.Cowboy.Req (binding, path, setHeader)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.EdgeAgent as EdgeAgent
import Rtsv2.EdgeAgentSup (maybeStartAndAddClient)
import Rtsv2.EdgeAgentSup as EdgeAgentSup
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.PoPDefinition (ServerAddress)
import Rtsv2.PoPDefinition as PoPDefintion
import Rtsv2.TransPoPAgent as Logger
import Shared.Stream (StreamId(..))
import Shared.Utils (lazyCrashIfMissing)
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

type ClientCountState = { streamId :: StreamId
                        , isAgentAvailable :: Boolean
                        , currentNodeHasEdge :: Boolean
                        }

clientCount :: StetsonHandler ClientCountState
clientCount =
  Rest.handler (\req ->
                 let streamId = StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                 in
                  do
                    thisNode <- PoPDefintion.thisNode
                    isAgentAvailable <- EdgeAgentSup.isAvailable
                    currentEdgeLocations <- fromMaybe Set.empty <$> IntraPoPAgent.whereIsEdge streamId
                    Rest.initResult req { streamId : streamId
                                        , isAgentAvailable
                                        , currentNodeHasEdge : member thisNode $ currentEdgeLocations
                                        }
               )
  # Rest.serviceAvailable (\req state@{isAgentAvailable} -> Rest.result isAgentAvailable req state)
  # Rest.resourceExists (\req state@{currentNodeHasEdge} -> Rest.result currentNodeHasEdge req state)
  # Rest.contentTypesProvided (\req state@{streamId} ->
                                  Rest.result
                                    ((tuple2 "text/plain" (content streamId)) : nil)
                                    req state)
  # Rest.yeeha

  where
    content streamId req state = do
      count <- EdgeAgent.currentClientCount streamId
      Rest.result (show count) req state
