module Rtsv2.Handler.Edge
       (
         clientCount
       ) where

import Prelude

import Data.Maybe (fromMaybe')
import Erl.Atom (atom)
import Erl.Cowboy.Req (binding)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.Agents.EdgeInstance as EdgeInstance
import Rtsv2.Agents.EdgeInstanceSup as EdgeInstanceSup
import Rtsv2.Agents.IntraPoP as IntraPoP
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
                              isAgentAvailable <- EdgeInstanceSup.isAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{streamId} -> do
                            thisNode <- PoPDefintion.thisNode
                            currentNodeHasEdge <- member thisNode <$> IntraPoP.whereIsEdge streamId
                            Rest.result currentNodeHasEdge req state)

  # Rest.contentTypesProvided (\req state@{streamId} ->
                                  Rest.result
                                    ((tuple2 "text/plain" (content streamId)) : nil)
                                    req state)
  # Rest.yeeha

  where
    content streamId req state = do
      count <- EdgeInstance.currentClientCount streamId
      Rest.result (show count) req state
