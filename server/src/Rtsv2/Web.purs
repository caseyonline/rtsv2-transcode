module Rtsv2.Web
  ( startLink
  )
  where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust)
import Data.Set (member)
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (Req, binding)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (Tuple2, Tuple4, tuple2, tuple4, uncurry4)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Rtsv2.Config as Config
import Rtsv2.EdgeAgent as EdgeAgent
import Rtsv2.EdgeAgentSup (startEdge)
import Rtsv2.EdgeAgentSup as EdgeAgentSup
import Rtsv2.Endpoints.Edge (edgeStart)
import Rtsv2.Endpoints.Health (healthCheck)
import Rtsv2.Env as Env
import Rtsv2.IngestAgentSup (startIngest)
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.PoPDefinition (ServerAddress)
import Rtsv2.PoPDefinition as PoPDefintion
import Rtsv2.TransPoPAgent as Logger
import Serf (Ip(..))
import Shared.Stream (StreamId(..), StreamVariantId(..))
import Shared.Utils (lazyCrashIfMissing)
import Stetson (RestResult, StetsonHandler)
import Stetson as Stetson
import Stetson.Rest as Rest

newtype State = State {}

serverName :: ServerName State Unit
serverName = Local "web"

startLink :: Config.WebConfig -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) Gen.defaultHandleInfo

init :: Config.WebConfig -> Effect State
init args = do
  bindIp <- Env.privateInterfaceIp
  Stetson.configure
    # Stetson.route "/api/transPoPLeader" transPoPLeader
    # Stetson.route "/api/healthCheck" healthCheck
    # Stetson.route "/api/client/:canary/edge/:stream_id/start" edgeStart
    --# Stetson.route "/api/client/:canary/edge/:stream_id/stop" edgeStop
    # Stetson.route "/api/client/:canary/ingest/:stream_id/:variant_id/start" ingestStart
    --# Stetson.route "/api/client/:canary/ingest/:stream_id/:variant_id/stop" ingestStop
    # Stetson.port args.port
    # (uncurry4 Stetson.bindTo) (ipToTuple bindIp)
    # Stetson.startClear "http_listener"
  pure $ State {}

ipToTuple :: Ip -> Tuple4 Int Int Int Int
ipToTuple (Ipv4 a b c d) = tuple4 a b c d

transPoPLeader :: StetsonHandler (Maybe ServerAddress)
transPoPLeader =
  Rest.handler (\req -> Rest.initResult req Nothing)
  # Rest.resourceExists (\req state -> do
                            currentLeader <- IntraPoPAgent.currentTransPoPLeader
                            Rest.result (isJust currentLeader) req currentLeader
                          )
  # Rest.contentTypesProvided (\req state ->
                                Rest.result ((tuple2 "text/plain" (\req2 currentLeader -> Rest.result (fromMaybe "" currentLeader) req2 state)) : nil) req state)
  # Rest.yeeha

type IngestState = { streamVariantId :: StreamVariantId }
ingestStart :: StetsonHandler IngestState
ingestStart =
  Rest.handler (\req ->
                 let streamId = fromMaybe' (lazyCrashIfMissing "stream_id binding missing") $ binding (atom "stream_id") req
                     variantId = fromMaybe' (lazyCrashIfMissing "variant_id binding missing") $ binding (atom "variant_id") req
                 in
                 Rest.initResult req {streamVariantId: StreamVariantId streamId variantId})
  -- # Rest.serviceAvailable (\req state -> do
  --                             registered <- Erl.isRegistered Agent.Ingest
  --                             Rest.result registered req state)
  -- # Rest.resourceExists (\req state@{streamVariantId} -> do
  --                           isAvailable <- isIngestActive streamVariantId
  --                           Rest.result isAvailable req state
  --                         )
  # Rest.contentTypesProvided (\req state -> do
                                  _ <- startIngest state.streamVariantId
                                  Rest.result (textWriter "" : nil) req state)
  # Rest.yeeha

emptyText  :: forall a. Tuple2 String (Req -> a -> (Effect (RestResult String a)))
emptyText = textWriter ""

textWriter :: forall a. String -> Tuple2 String (Req -> a -> (Effect (RestResult String a)))
textWriter text = tuple2 "text/plain" (\req state -> Rest.result text req state)
