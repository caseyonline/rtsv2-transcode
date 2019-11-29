module Rtsv2.Web
  ( startLink
  , Config
  )
  where

import Prelude

import Control.Bind (join)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.String (Pattern(..), split)
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (Req, binding)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (Tuple2, Tuple4, tuple2, tuple4, uncurry4)
import Gproc as Gproc
import LocalPopState as PopState
import Os as Os
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Rtsv2.EnvConfig as Env
import Serf (Ip(..), strToIp)
import Shared.Agents as Agents
import Shared.Utils (lazyCrashIfMissing)
import Stetson (RestResult, StetsonHandler)
import Stetson as Stetson
import Stetson.Rest as Rest

newtype State = State {}

-- TODO - config should include BindIFace or BindIp
type Config = { port :: Int }

serverName :: ServerName State Unit
serverName = Local "web"

startLink :: Config -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) Gen.defaultHandleInfo

init :: Config -> Effect State
init args = do
  bindIp <- Env.privateInterfaceIp
  Stetson.configure
    # Stetson.route "/poc/api/client/:canary/edge/:stream_id/connect" edge_entrypoint
    # Stetson.route "/test/alive" alive_entrypoint
    # Stetson.port args.port
    # (uncurry4 Stetson.bindTo) (ipToTuple bindIp)
    # Stetson.startClear "http_listener"
  pure $ State {}

ipToTuple :: Ip -> Tuple4 Int Int Int Int
ipToTuple (Ipv4 a b c d) = tuple4 a b c d

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
