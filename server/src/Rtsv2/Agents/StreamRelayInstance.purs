module Rtsv2.Agents.StreamRelayInstance
  ( startLink
  , isAvailable
  , registerEgest
  , init
  , status
  , Status
  , CreateRelayPayload
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Rtsv2.Agents.TransPoP (ViaPoPs)
import Rtsv2.Names as Names
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (EgestServer, PoPName, RelayServer, Server)


type CreateRelayPayload
  = { streamId :: StreamId
    , aggregator :: Server
    }


type Status = { }
type State
  = { streamId :: StreamId
    , aggregator :: Server
    -- , primaryRelayRoutes :: Maybe (List ViaPoPs)
    -- , primaryNeighbours :: Maybe (List Server)
    -- , secondaryRelayCount :: Int
    -- , secondaryRelayRoutes :: Maybe (List ViaPoPs)
    , relaysServed :: Set RelayServer
    , egestsServed :: Set EgestServer
    , egestSourceRoutes :: Maybe (List ViaPoPs)
    , status :: Status
    }

serverName :: StreamId -> ServerName State Unit
serverName streamId = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") (tuple2 "streamRelay" streamId))

startLink :: CreateRelayPayload -> Effect StartLinkResult
startLink payload = Gen.startLink (serverName payload.streamId) (init payload) Gen.defaultHandleInfo

isAvailable :: StreamId -> Effect Boolean
isAvailable streamId = Names.isRegistered (serverName streamId)

status  :: StreamId -> Effect Status
status streamId =
  exposeStateMember _.status streamId


init :: CreateRelayPayload -> Effect State
init payload = do
  _ <- logInfo "StreamRelay starting" {payload}
  pure { streamId: payload.streamId
       , aggregator : payload.aggregator
       , relaysServed : mempty
       , egestsServed : mempty
       , egestSourceRoutes : Nothing
       , status: {}
       }

registerEgest :: StreamId -> EgestServer -> Effect Unit
registerEgest streamId egestServer = Gen.doCall (serverName streamId) $ doRegisterEgest egestServer

doRegisterEgest :: EgestServer -> State -> Effect (CallResult Unit State)
doRegisterEgest egestServer state@{egestsServed} = do
  _ <- logInfo "Register egest " {egestServer}
  pure $ CallReply unit state{ egestsServed = Set.insert egestServer egestsServed
                             }


exposeStateMember :: forall a. (State -> a) -> StreamId -> Effect a
exposeStateMember member streamId = Gen.doCall (serverName streamId)
  \state -> pure $ CallReply (member state) state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom <$> (show Agent.StreamRelay :  "Instance" : nil)

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

--logWarning :: forall a. Logger a
--logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
