module Rtsv2.Endpoints.Edge
       (
         clientCount
       ) where

import Prelude

import Data.Maybe (fromMaybe')
import Erl.Atom (atom)
import Erl.Cowboy.Req (binding)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.EdgeAgent as EdgeAgent
import Rtsv2.EdgeAgentSup as EdgeAgentSup
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.PoPDefinition as PoPDefintion
import Rtsv2.Utils (member)
import Shared.Stream (StreamId(..))
import Shared.Utils (lazyCrashIfMissing)
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

type ClientCountState = { streamId :: StreamId
                        }

clientCount :: StetsonHandler ClientCountState
clientCount =
  Rest.handler (\req ->
                 let streamId = StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                 in
                    Rest.initResult req { streamId : streamId }
               )
  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- EdgeAgentSup.isAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{streamId} -> do
                            thisNode <- PoPDefintion.thisNode
                            currentNodeHasEdge <- member thisNode <$> IntraPoPAgent.whereIsEdge streamId
                            Rest.result currentNodeHasEdge req state)

  # Rest.contentTypesProvided (\req state@{streamId} ->
                                  Rest.result
                                    ((tuple2 "text/plain" (content streamId)) : nil)
                                    req state)
  # Rest.yeeha

  where
    content streamId req state = do
      count <- EdgeAgent.currentClientCount streamId
      Rest.result (show count) req state
