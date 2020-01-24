module Rtsv2.Web
  ( startLink
  )
  where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Req (Req)
import Erl.Cowboy.Routes (InitialState(..), Path(..), matchSpec)
import Erl.Data.List (List, nil, singleton, (:))
import Erl.Data.Tuple (Tuple2, Tuple4, tuple2, tuple3, tuple4, uncurry4)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen as Gen
import Rtsv2.Agents.IntraPoP as IntraPoPAgent
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Handler.Client as ClientHandler
import Rtsv2.Handler.EgestStats as EgestStatsHandler
import Rtsv2.Handler.Health as HealthHandler
import Rtsv2.Handler.Ingest as IngestHandler
import Rtsv2.Handler.IngestAggregator as IngestAggregatorHandler
import Rtsv2.Handler.LlnwStub as LlnwStubHandler
import Rtsv2.Handler.Load as LoadHandler
import Rtsv2.Handler.Relay as RelayHandler
import Rtsv2.Names as Names
import Rtsv2.Router.Endpoint (Endpoint(..), endpoint)
import Rtsv2.Router.Parser (printUrl)
import Serf (Ip(..))
import Shared.Stream (StreamAndVariant(..), StreamId(..), StreamVariant(..))
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
    # Stetson.route
        (printUrl endpoint TransPoPLeaderE)
        transPoPLeader
    # Stetson.route
        (printUrl endpoint HealthCheckE)
        HealthHandler.healthCheck
    # Stetson.route
        (printUrl endpoint (EgestStatsE (StreamId ":stream_id")))
        EgestStatsHandler.stats
    # Stetson.route
        (printUrl endpoint RelayE)
        RelayHandler.resource
    # Stetson.route
        (printUrl endpoint (RelayStatsE(StreamId ":stream_id")))
        RelayHandler.resource
    # Stetson.route
        (printUrl endpoint LoadE)
        LoadHandler.load
    # Stetson.route
        (printUrl endpoint (IngestAggregatorE $ StreamId ":stream_id"))
        IngestAggregatorHandler.ingestAggregator
    # Stetson.route
        (printUrl endpoint (IngestAggregatorActiveIngestsE (StreamId ":stream_id") (StreamVariant ":variant_id")))
        IngestAggregatorHandler.ingestAggregatorsActiveIngest
    # Stetson.route
        (printUrl endpoint IngestAggregatorsE)
        IngestAggregatorHandler.ingestAggregators
    # Stetson.route
        (printUrl endpoint (IngestStartE ":canary" ":short_name" (StreamVariant ":variant_id")))
        IngestHandler.ingestStart
    # Stetson.route
        (printUrl endpoint (IngestStopE ":canary" ":short_name" (StreamVariant ":variant_id")))
        IngestHandler.ingestStop
    # Stetson.route
        (printUrl endpoint (ClientStartE ":canary" (StreamId ":stream_id")))
        ClientHandler.clientStart
    # Stetson.route
        (printUrl endpoint (ClientStopE ":canary" (StreamId ":stream_id")))
        ClientHandler.clientStop
    # Stetson.route
        (printUrl endpoint StreamAuthE)
        LlnwStubHandler.streamAuthType
    # Stetson.route
        (printUrl endpoint StreamAuthTypeE)
        LlnwStubHandler.streamAuth
    # Stetson.route
        (printUrl endpoint StreamPublishE)
        LlnwStubHandler.streamPublish
    # Stetson.static
         (printUrl endpoint (IngestAggregatorPlayerE (StreamId ":stream_id")))
         (PrivFile "rtsv2" "www/aggregatorPlayer.html")
    # Stetson.static
         ((printUrl endpoint (IngestAggregatorPlayerJsE (StreamId ":stream_id"))) <> "/[...]")
         (PrivDir "rtsv2" "www/js")
    # Stetson.static
         (printUrl endpoint (IngestAggregatorActiveIngestsPlayerE (StreamId ":stream_id") (StreamVariant ":variant_id")))
         (PrivFile "rtsv2" "www/play.html")
    # Stetson.static
         ((printUrl endpoint (IngestAggregatorActiveIngestsPlayerJsE (StreamId ":stream_id") (StreamVariant ":variant_id"))) <> "/[...]")
         (PrivDir "rtsv2" "www/js")

    # Stetson.cowboyRoutes cowboyRoutes
    # Stetson.port args.port
    # (uncurry4 Stetson.bindTo) (ipToTuple bindIp)
    # Stetson.startClear "http_listener"
  pure $ State {}
  where
    cowboyRoutes :: List Path
    cowboyRoutes =
      Path (tuple3
            (matchSpec $ printUrl endpoint (IngestInstanceLlwpE (StreamId ":stream_id") (StreamVariant ":variant_id")))
            (NativeModuleName $ atom "llwp_stream_resource")
            (InitialState $ unsafeToForeign makeStreamAndVariant)
           )
      : Path (tuple3
              (matchSpec $ printUrl endpoint (IngestAggregatorActiveIngestsPlayerSessionStartE (StreamId ":stream_id") (StreamVariant ":variant_id")))
              (NativeModuleName $ atom "rtsv2_webrtc_session_start_resource")
              (InitialState $ unsafeToForeign makeStreamAndVariant)
             )
      : Path (tuple3
              (matchSpec $ printUrl endpoint (IngestAggregatorActiveIngestsPlayerSessionE (StreamId ":stream_id") (StreamVariant ":variant_id") ":session_id"))
              (NativeModuleName $ atom "rtsv2_webrtc_session_resource")
              (InitialState $ unsafeToForeign makeStreamAndVariant)
             )
      : nil

    makeStreamAndVariant :: String -> String -> StreamAndVariant
    makeStreamAndVariant streamId variantId = StreamAndVariant (wrap streamId) (wrap variantId)

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


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = serverName # Names.toDomain # singleton

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
