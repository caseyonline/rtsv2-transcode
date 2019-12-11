module Rtsv2.Web
  ( startLink
  )
  where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, isJust)
--import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (Req)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (Tuple2, Tuple4, tuple2, tuple4, uncurry4)
import Foreign (Foreign)
import Logger (info) as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Record as Record
import Rtsv2.Config as Config
import Rtsv2.Endpoints.Client as ClientEndpoint
import Rtsv2.Endpoints.Edge as EdgeEndpoint
import Rtsv2.Endpoints.Ingest as IngestEndpoint
import Rtsv2.Endpoints.Health (healthCheck)
import Rtsv2.Env as Env
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.PoPDefinition (ServerAddress)
import Serf (Ip(..))
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

    # Stetson.route "/api/client/:canary/ingest/:stream_id/:variant_id/start" IngestEndpoint.ingestStart
    # Stetson.route "/api/client/:canary/ingest/:stream_id/:variant_id/stop" IngestEndpoint.ingestStop

    # Stetson.route "/api/client/:canary/client/:stream_id/start" ClientEndpoint.clientStart
    # Stetson.route "/api/client/:canary/client/:stream_id/stop" ClientEndpoint.clientStop

    # Stetson.route "/api/client/:canary/edge/:stream_id/clientCount" EdgeEndpoint.clientCount

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

emptyText  :: forall a. Tuple2 String (Req -> a -> (Effect (RestResult String a)))
emptyText = textWriter ""

textWriter :: forall a. String -> Tuple2 String (Req -> a -> (Effect (RestResult String a)))
textWriter text = tuple2 "text/plain" (\req state -> Rest.result text req state)


logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom "Web") : nil) } { misc: metaData })
