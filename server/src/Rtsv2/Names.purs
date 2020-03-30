module Rtsv2.Names
       (
         agentSupName
       , egestInstanceName
       , egestInstanceSupName
       , egestInstanceStateName

       , ingestAggregatorSupName
       , ingestAggregatorInstanceStateName
       , ingestAggregatorInstanceSupName
       , ingestAggregatorInstanceName

       , ingestInstanceSupName
       , ingestInstanceStateName
       , ingestInstanceName
       , ingestRtmpServerName
       , ingestRtmpCryptoName

       , ingestStatsName
       , ingestSupName
       , intraPoPName
       , loadServerName
       , popDefinitionName

       , streamRelaySupName
       , streamRelayInstanceStateName
       , streamRelayInstanceSupName
       , streamRelayInstanceName
       , streamRelayDownstreamProxyName

       , transPoPName
       , webServerName
       , toDomain

       , gprocName
       )
       where

import Prelude

import Erl.Atom (Atom, atom)
import Erl.Data.Tuple (tuple2, tuple3, tuple4)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Pinto (ServerName(..), SupervisorName)
import Shared.Agent (Agent(..))
import Shared.Stream (AggregatorKey, EgestKey, IngestKey, RelayKey)
import Shared.Types (PoPName)

agentSupName :: SupervisorName
agentSupName = Local (atom "AgentSup")

egestInstanceName :: forall a b. EgestKey -> ServerName a b
egestInstanceName = gprocName2 Egest

egestInstanceSupName :: SupervisorName
egestInstanceSupName = instanceSup Egest

egestInstanceStateName :: forall a b. EgestKey -> ServerName a b
egestInstanceStateName = gprocName2WithSuffix "State" Egest

ingestAggregatorSupName :: SupervisorName
ingestAggregatorSupName = sup IngestAggregator

ingestAggregatorInstanceStateName :: forall a b. AggregatorKey -> ServerName a b
ingestAggregatorInstanceStateName = gprocName2WithSuffix "State" IngestAggregator

ingestAggregatorInstanceSupName :: AggregatorKey -> SupervisorName
ingestAggregatorInstanceSupName = gprocInstanceSup IngestAggregator

ingestAggregatorInstanceName :: forall a b. AggregatorKey -> ServerName a b
ingestAggregatorInstanceName = gprocName2 IngestAggregator

ingestInstanceSupName :: SupervisorName
ingestInstanceSupName = instanceSup Ingest

ingestInstanceStateName :: forall a b. IngestKey -> ServerName a b
ingestInstanceStateName = gprocName2WithSuffix "State" Ingest

ingestInstanceName :: forall a b. IngestKey -> ServerName a b
ingestInstanceName = gprocName2 Ingest

ingestRtmpServerName :: forall a b. ServerName a b
ingestRtmpServerName = withSuffix "RtmpServer" Ingest

ingestRtmpCryptoName :: forall a b. ServerName a b
ingestRtmpCryptoName = withSuffix "RtmpCrypto" Ingest

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

streamRelaySupName :: SupervisorName
streamRelaySupName = sup StreamRelay

streamRelayInstanceStateName :: forall a b. RelayKey -> ServerName a b
streamRelayInstanceStateName = gprocName2WithSuffix "State" StreamRelay

streamRelayInstanceSupName :: RelayKey -> SupervisorName
streamRelayInstanceSupName = gprocInstanceSup StreamRelay

streamRelayInstanceName :: forall a b. RelayKey -> ServerName a b
streamRelayInstanceName = gprocName2 StreamRelay

streamRelayDownstreamProxyName :: forall a b. RelayKey -> PoPName -> ServerName a b
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

gprocInstanceSup :: forall t x. Show t => t -> x -> SupervisorName
gprocInstanceSup t = gprocName <<< tuple2 ((show t) <> "InstanceSup")

sup :: forall a. Show a => a -> SupervisorName
sup = withSuffix "Sup"

gprocName :: forall t a b. t -> ServerName a b
gprocName term =
    Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") term)

gprocName2 :: forall a b t x. Show t => t -> x -> ServerName a b
gprocName2 t = gprocName <<< tuple2 (show t)

gprocName2WithSuffix :: forall a b t x. Show t => String -> t -> x -> ServerName a b
gprocName2WithSuffix suffix t = gprocName <<< tuple2 ((show t) <> suffix)

gprocName3 :: forall a b t x y. Show t => t -> x -> y -> ServerName a b
gprocName3 t x y = gprocName $ tuple3 (show t) x y

gprocName4 :: forall a b t x y z. Show t => t -> x -> y -> z -> ServerName a b
gprocName4 t x y z = gprocName $ tuple4 (show t) x y z
