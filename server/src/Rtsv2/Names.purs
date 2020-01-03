module Rtsv2.Names
       (
         streamRelayAgentSupName
       , intraPoPAgentName
       , transPoPAgentName
       , popDefinitionName
       , edgeAgentSupName
       , edgeInstanceName
       , edgeInstanceRegistered
       , ingestAgentSupName
       , ingestAgentSupRegistered
       , ingestAgentInstanceSupName
       , ingestAggregatorAgentSupName
       , ingestAggregatorAgentSupRegistered
       , ingestAggregatorInstanceName
       , ingestRtmpServerName
       , ingestRtmpServerRegistered
       , loadServerName
       , webServerName
       )
       where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Utils as ErlUtils
import Foreign (unsafeToForeign)
import Gproc as Gproc
import Pinto (ServerName(..))
import Shared.Agent (Agent(..))
import Shared.Agent as SharedAgents
import Shared.Stream (StreamId)

intraPoPAgentName :: forall a b. ServerName a b
intraPoPAgentName = Local $ show SharedAgents.IntraPoP

transPoPAgentName :: forall a b. ServerName a b 
transPoPAgentName = Local $ show SharedAgents.TransPoP

popDefinitionName :: forall a b. ServerName a b
popDefinitionName = Local "PoPDefinition"

streamRelayAgentSupName :: String 
streamRelayAgentSupName = show SharedAgents.StreamRelay

edgeAgentSupName :: String
edgeAgentSupName = show Edge

edgeInstanceName :: forall a b. StreamId -> ServerName a b
edgeInstanceName streamId = gprocName (tuple2 (show Edge) streamId)

edgeInstanceRegistered :: StreamId -> Effect Boolean
edgeInstanceRegistered streamId = Gproc.isRegistered (tuple2 (show Edge) streamId)

loadServerName :: forall a b. ServerName a b
loadServerName = Local "load"

webServerName :: forall a b. ServerName a b
webServerName = Local "web"

ingestAgentSupName :: String
ingestAgentSupName = "ingestAgentSup"

ingestAgentSupRegistered :: Effect Boolean
ingestAgentSupRegistered = ErlUtils.isRegistered "ingestAgentSup"

ingestAggregatorAgentSupName :: String
ingestAggregatorAgentSupName = "ingestAggregatorAgentSup"

ingestAggregatorAgentSupRegistered :: Effect Boolean
ingestAggregatorAgentSupRegistered = ErlUtils.isRegistered "ingestAggregatorAgentSup"

ingestAgentInstanceSupName :: String
ingestAgentInstanceSupName = "ingestAgentInstanceSup"

ingestRtmpServerName :: forall a b. ServerName a b
ingestRtmpServerName = Local "ingestRtmpServer"

ingestRtmpServerRegistered :: Effect Boolean
ingestRtmpServerRegistered = ErlUtils.isRegistered "ingestRtmpServer"

ingestAggregatorInstanceName :: forall a b. StreamId -> ServerName a b
ingestAggregatorInstanceName streamId =
  gprocName (tuple2 (show IngestAggregator) streamId)

gprocName :: forall t a b. t -> ServerName a b
gprocName term =
    Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") term)
