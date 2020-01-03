module Rtsv2.Names
       (
         agentSupName
       , edgeInstanceName
       , edgeInstanceSupName
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

import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Utils as ErlUtils
import Foreign (unsafeToForeign)
import Gproc as Gproc
import Pinto (ServerName(..), SupervisorName)
import Shared.Agent (Agent(..))
import Shared.Agent as SharedAgents
import Shared.Stream (StreamId, StreamVariantId)

agentSupName :: SupervisorName
agentSupName = Local "agentSup"

edgeInstanceName :: forall a b. StreamId -> ServerName a b
edgeInstanceName streamId = gprocName (tuple2 (show Edge) streamId)

edgeInstanceSupName :: SupervisorName
edgeInstanceSupName = Local $ (show Edge) <> "InstanceSup"

ingestAggregatorInstanceName :: forall a b. StreamId -> ServerName a b
ingestAggregatorInstanceName streamId = gprocName (tuple2 (show IngestAggregator) streamId)

ingestAggregatorInstanceSupName :: SupervisorName
ingestAggregatorInstanceSupName = Local $ (show IngestAggregator) <> "InstanceSup"

ingestInstanceName :: forall a b. StreamVariantId -> ServerName a b
ingestInstanceName streamVariantId = gprocName (tuple2 (show Ingest) streamVariantId)

ingestInstanceSupName :: SupervisorName
ingestInstanceSupName = Local $ (show Ingest) <> "InstanceSup"

ingestRtmpServerName :: forall a b. ServerName a b
ingestRtmpServerName = Local $ (show Ingest) <> "RtmpServer"

ingestSupName :: SupervisorName
ingestSupName = Local $ (show Ingest) <> "Sup"

intraPoPName :: forall a b. ServerName a b
intraPoPName = Local $ show SharedAgents.IntraPoP

loadServerName :: forall a b. ServerName a b
loadServerName = Local "load"

popDefinitionName :: forall a b. ServerName a b
popDefinitionName = Local "PoPDefinition"

streamRelayInstanceSupName :: SupervisorName
streamRelayInstanceSupName = Local $ (show SharedAgents.StreamRelay) <> "InstanceSup"

transPoPName :: forall a b. ServerName a b 
transPoPName = Local $ show SharedAgents.TransPoP

webServerName :: forall a b. ServerName a b
webServerName = Local "web"

isRegistered :: forall a b. ServerName a b -> Effect Boolean
isRegistered (Local name) = ErlUtils.isRegistered name
isRegistered (Global name) = ErlUtils.isRegistered name
isRegistered (Via (NativeModuleName m) name) = pure $ viaIsRegisteredImpl m name

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------
foreign import viaIsRegisteredImpl :: forall a. Atom -> a -> Boolean

gprocName :: forall t a b. t -> ServerName a b
gprocName term =
    Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") term)
