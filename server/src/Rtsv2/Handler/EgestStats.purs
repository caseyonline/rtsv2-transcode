module Rtsv2.Handler.EgestStats
       (
         stats
       ) where

import Prelude

import Data.Maybe (fromMaybe')
import Erl.Atom (atom)
import Erl.Cowboy.Req (binding)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.PoPDefinition as PoPDefintion
import Rtsv2.Utils (member)
import Shared.Stream (StreamId(..))
import Shared.Types (ServerLoad(..), locatedServerAddress)
import Shared.Utils (lazyCrashIfMissing)
import Stetson (StetsonHandler)
import Stetson.Rest as Rest

type StatsState = { streamId :: StreamId
                        }

stats :: StetsonHandler StatsState
stats =
  Rest.handler (\req ->
                 let streamId = StreamId $ fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                 in
                    Rest.initResult req { streamId : streamId }
               )
  # Rest.serviceAvailable (\req state -> do
                              isAgentAvailable <- EgestInstanceSup.isAvailable
                              Rest.result isAgentAvailable req state)

  # Rest.resourceExists (\req state@{streamId} -> do
                            thisNode <- PoPDefintion.thisNode
                            serverLoads <- IntraPoP.whereIsEgest streamId
                            let currentNodeHasEgest = member thisNode $ (\(ServerLoad locatedServer _) -> locatedServerAddress locatedServer) <$> serverLoads
                            Rest.result currentNodeHasEgest req state)

  # Rest.contentTypesProvided (\req state@{streamId} ->
                                  Rest.result
                                    ((tuple2 "text/plain" (content streamId)) : nil)
                                    req state)
  # Rest.yeeha

  where
    content streamId req state = do
      count <- EgestInstance.currentStats streamId
      Rest.result (show count) req state
