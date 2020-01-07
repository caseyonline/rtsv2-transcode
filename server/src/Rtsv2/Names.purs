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
       , ingestSupName
       , intraPoPName
       , isRegistered
       , loadServerName
       , popDefinitionName
       , streamRelayInstanceSupName
       , transPoPName
       , webServerName
       )
       where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Utils as ErlUtils
import Foreign (unsafeToForeign)
import Pinto (ServerName(..), SupervisorName)
import Shared.Agent (Agent(..))
import Shared.Stream (StreamId, StreamAndVariant)

agentSupName :: SupervisorName
agentSupName = Local "AgentSup"

egestInstanceName :: forall a b. StreamId -> ServerName a b
egestInstanceName = gprocName2 Egest

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

ingestSupName :: SupervisorName
ingestSupName = sup Ingest

intraPoPName :: forall a b. ServerName a b
intraPoPName = localName IntraPoP

loadServerName :: forall a b. ServerName a b
loadServerName = Local "Load"

popDefinitionName :: forall a b. ServerName a b
popDefinitionName = Local "PoPDefinition"

streamRelayInstanceSupName :: SupervisorName
streamRelayInstanceSupName = instanceSup StreamRelay

transPoPName :: forall a b. ServerName a b
transPoPName = localName TransPoP

webServerName :: forall a b. ServerName a b
webServerName = Local "Web"

isRegistered :: forall a b. ServerName a b -> Effect Boolean
isRegistered (Local name) = ErlUtils.isRegistered name
isRegistered (Global name) = ErlUtils.isRegistered name
isRegistered (Via (NativeModuleName m) name) = viaIsRegisteredImpl m name

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------
foreign import viaIsRegisteredImpl :: forall a. Atom -> a -> Effect Boolean

withSuffix :: forall a b t. Show t => String -> t -> ServerName a b
withSuffix suffix t = Local $ (show t) <> suffix

localName :: forall a b t. Show t => t -> ServerName a b
localName = Local <<< show

instanceSup :: forall a. Show a => a -> SupervisorName
instanceSup = withSuffix "InstanceSup"

sup :: forall a. Show a => a -> SupervisorName
sup = withSuffix "Sup"

gprocName :: forall t a b. t -> ServerName a b
gprocName term =
    Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") term)

gprocName2 :: forall a b t x. Show t => t -> x -> ServerName a b
gprocName2 t = gprocName <<< tuple2 (show t)
