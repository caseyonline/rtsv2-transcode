module Rtsv2.Names
       (
         agentSupName
       , egestInstanceName
       , egestInstanceSupName
       , ingestAggregatorInstanceName
       , ingestAggregatorInstanceSupName
       , ingestInstanceName
       , ingestInstanceSupName
       , ingestRtmpServerName
       , ingestStatsName
       , ingestSupName
       , intraPoPName
       , loadServerName
       , popDefinitionName
       , streamRelayDownstreamProxyName
       , streamRelayInstanceName
       , streamRelayInstanceSupName
       , transPoPName
       , webServerName
       , toDomain
       )
       where

import Prelude

import Erl.Atom (Atom, atom)
import Erl.Data.Tuple (tuple2, tuple3, tuple4)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Pinto (ServerName(..), SupervisorName)
import Shared.Agent (Agent(..))
import Shared.Stream (StreamId, StreamAndVariant)
import Shared.Types (PoPName)

agentSupName :: SupervisorName
agentSupName = Local (atom "AgentSup")

egestInstanceName :: forall a b. StreamId -> ServerName a b
egestInstanceName = gprocName2 Egest

-- egestProxyName :: forall a b. StreamId -> Server -> ServerName a b
-- egestProxyName = gprocProxyName3 Egest

-- egestProxyMatch :: StreamId -> Tuple4 String String StreamId Atom
-- egestProxyMatch streamId = tuple4 (show Egest) "proxy" streamId (atom "$1")

egestInstanceSupName :: SupervisorName
egestInstanceSupName = instanceSup Egest

ingestAggregatorInstanceName :: forall a b. StreamId -> ServerName a b
ingestAggregatorInstanceName = gprocName2 IngestAggregator

ingestAggregatorInstanceSupName :: SupervisorName
ingestAggregatorInstanceSupName = instanceSup IngestAggregator

ingestInstanceName :: forall a b. StreamAndVariant -> ServerName a b
ingestInstanceName = gprocName2 Ingest

ingestInstanceSupName :: SupervisorName
ingestInstanceSupName = instanceSup Ingest

ingestRtmpServerName :: forall a b. ServerName a b
ingestRtmpServerName = withSuffix "RtmpServer" Ingest

ingestStatsName :: forall a b. ServerName a b
ingestStatsName = withSuffix "Stats" Ingest

ingestSupName :: SupervisorName
ingestSupName = sup Ingest

intraPoPName :: forall a b. ServerName a b
intraPoPName = localName IntraPoP

loadServerName :: forall a b. ServerName a b
loadServerName = Local (atom "Load")

popDefinitionName :: forall a b. ServerName a b
popDefinitionName = Local (atom "PoPDefinition")

streamRelayInstanceSupName :: SupervisorName
streamRelayInstanceSupName = instanceSup StreamRelay

streamRelayInstanceName :: forall a b. StreamId -> ServerName a b
streamRelayInstanceName = gprocName2 StreamRelay

streamRelayDownstreamProxyName :: forall a b. StreamId -> PoPName -> ServerName a b
streamRelayDownstreamProxyName = gprocName4 StreamRelay "proxy"

transPoPName :: forall a b. ServerName a b
transPoPName = localName TransPoP

webServerName :: forall a b. ServerName a b
webServerName = Local (atom "Web")

toDomain :: forall a b. ServerName a b -> Atom
toDomain (Local name) = name
toDomain (Global name) = name
toDomain _ = atom "gproc domain unknown"

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------
withSuffix :: forall a b t. Show t => String -> t -> ServerName a b
withSuffix suffix t = Local $ atom $ (show t) <> suffix

localName :: forall a b t. Show t => t -> ServerName a b
localName = Local <<< atom <<< show

instanceSup :: forall a. Show a => a -> SupervisorName
instanceSup = withSuffix "InstanceSup"

sup :: forall a. Show a => a -> SupervisorName
sup = withSuffix "Sup"

gprocName :: forall t a b. t -> ServerName a b
gprocName term =
    Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") term)

gprocName2 :: forall a b t x. Show t => t -> x -> ServerName a b
gprocName2 t = gprocName <<< tuple2 (show t)

gprocName3 :: forall a b t x y. Show t => t -> x -> y -> ServerName a b
gprocName3 t x y = gprocName $ tuple3 (show t) x y

gprocName4 :: forall a b t x y z. Show t => t -> x -> y -> z -> ServerName a b
gprocName4 t x y z = gprocName $ tuple4 (show t) x y z
