module Rtsv2.Web
  ( startLink
  , Config
  )
  where

import Prelude

import Data.Maybe (fromMaybe')
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (Req, binding)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import LocalPopState as PopState
import Gproc as Gproc
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Shared.Agents as Agents
import Shared.Utils (lazyCrashIfMissing)
import Stetson (RestResult, StetsonHandler)
import Stetson as Stetson
import Stetson.Rest as Rest

newtype State = State {}

type Config = { port :: Int }

serverName :: ServerName State Unit
serverName = Local "web"

startLink :: Config -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) Gen.defaultHandleInfo

init :: Config -> Effect State
init args = do
  Stetson.configure
    # Stetson.route "/poc/api/client/:canary/edge/:stream_id/connect" edge_entrypoint
    # Stetson.route "/test/alive" alive_entrypoint
    # Stetson.port args.port
    # Stetson.bindTo 0 0 0 0
    # Stetson.startClear "http_listener"
  pure $ State {}


alive_entrypoint :: StetsonHandler Unit
alive_entrypoint =
  Rest.handler (\req -> Rest.initResult req unit)
  # Rest.contentTypesProvided (\req state -> Rest.result (emptyText : nil) req unit)
  # Rest.yeeha



type EdgeState = { streamId :: String }
edge_entrypoint :: StetsonHandler EdgeState
edge_entrypoint =
  Rest.handler (\req ->
                 let streamId = fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                 in
                 Rest.initResult req {streamId})
  # Rest.serviceAvailable (\req state -> do
                              registered <- Gproc.isRegistered Agents.EdgeAgent
                              Rest.result registered req state)
  # Rest.resourceExists (\req state@{streamId} -> do
                            isAvailable <- PopState.isStreamAvailable streamId
                            Rest.result isAvailable req state
                          )
  # Rest.contentTypesProvided (\req state -> Rest.result (textWriter "" : nil) req state)
  # Rest.yeeha


emptyText  :: forall a. Tuple2 String (Req -> a -> (Effect (RestResult String a)))
emptyText = tuple2 "text/plain" (\req state -> Rest.result "" req state)

textWriter :: forall a. String -> Tuple2 String (Req -> a -> (Effect (RestResult String a)))
textWriter t = tuple2 "text/plain" (\req state -> Rest.result t req state)
