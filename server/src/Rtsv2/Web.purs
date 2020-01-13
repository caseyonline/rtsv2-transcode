module Rtsv2.Web
  ( startLink
  )
  where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy.Req (Req)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (Tuple2, Tuple4, tuple2, tuple4, uncurry4)
import Foreign (Foreign)
import Logger (info) as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen as Gen
import Record as Record
import Rtsv2.Agents.IntraPoP as IntraPoPAgent
import Rtsv2.Config as Config
import Rtsv2.Endpoints.Client as ClientEndpoint
import Rtsv2.Endpoints.Edge as EdgeEndpoint
import Rtsv2.Endpoints.Health as HealthEndpoint
import Rtsv2.Endpoints.Ingest as IngestEndpoint
import Rtsv2.Endpoints.IngestAggregator as IngestAggregatorEndpoint
import Rtsv2.Endpoints.LlnwStub as LlnwStub
import Rtsv2.Endpoints.Load as LoadEndpoint
import Rtsv2.Env as Env
import Rtsv2.Names as Names
import Serf (Ip(..))
import Shared.Types (ServerAddress)
import Stetson (RestResult, StaticAssetLocation(..), StetsonHandler)
import Stetson as Stetson
import Stetson.Rest as Rest

newtype State = State {}


serverName :: ServerName State Unit
serverName = Names.webServerName

startLink :: Config.WebConfig -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) Gen.defaultHandleInfo

init :: Config.WebConfig -> Effect State
init args = do
  bindIp <- Env.privateInterfaceIp
  Stetson.configure
    # Stetson.route "/api/transPoPLeader" transPoPLeader
    # Stetson.route "/api/healthCheck" HealthEndpoint.healthCheck
    # Stetson.route "/api/load" LoadEndpoint.load
    # Stetson.route "/api/agents/ingestAggregator/:stream_id" IngestAggregatorEndpoint.ingestAggregator

    # Stetson.route "/api/client/:canary/ingest/:stream_id/:variant_id/start" IngestEndpoint.ingestStart
    # Stetson.route "/api/client/:canary/ingest/:stream_id/:variant_id/stop" IngestEndpoint.ingestStop

    # Stetson.route "/api/client/:canary/client/:stream_id/start" ClientEndpoint.clientStart
    # Stetson.route "/api/client/:canary/client/:stream_id/stop" ClientEndpoint.clientStop

    # Stetson.route "/api/client/:canary/edge/:stream_id/clientCount" EdgeEndpoint.clientCount

    # Stetson.route "/llnwstub/rts/v1/streamauthtype" LlnwStub.streamAuthType
    # Stetson.route "/llnwstub/rts/v1/streamauth" LlnwStub.streamAuth
    # Stetson.route "/llnwstub/rts/v1/streampublish" LlnwStub.streamPublish

    # Stetson.static "/assets/[...]" (PrivDir Config.appName "www/assets")
    # Stetson.static "/adminApp" (PrivFile Config.appName "www/index.html")
    # Stetson.static "/adminApp/[...]" (PrivFile Config.appName "www/index.html")

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
                                Rest.result ((tuple2 "text/plain" (\req2 currentLeader -> Rest.result (fromMaybe "" (unwrap <$> currentLeader)) req2 state)) : nil) req state)
  # Rest.yeeha

emptyText  :: forall a. Tuple2 String (Req -> a -> (Effect (RestResult String a)))
emptyText = textWriter ""

textWriter :: forall a. String -> Tuple2 String (Req -> a -> (Effect (RestResult String a)))
textWriter text = tuple2 "text/plain" (\req state -> Rest.result text req state)


logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom "Web") : nil) } { misc: metaData })
