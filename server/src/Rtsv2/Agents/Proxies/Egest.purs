module Rtsv2.Agents.Proxies.Egest
       ( whereIsLocal
       , whereIsRemote
       , startLink
       , findEgestForStream
       , ProxyMode
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, head, nil, (:))
import Gproc as Gproc
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Rtsv2.Utils (crashIfLeft)
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (EgestLocation(..), FailureReason(..), Server, extractAddress, serverLoadToServer)
import SpudGun as SpudGun

data ProxyMode = New | Existing
type State = { streamId :: StreamId
             , forServer :: Server
             , mode :: ProxyMode
             }

type Msg = Unit

whereIsRemote :: StreamId -> Effect (List (ServerName State Msg))
whereIsRemote streamId = do
  Gproc.match $ Names.egestRemoteProxyMatch streamId


whereIsLocal :: StreamId -> Effect (Maybe (ServerName State Msg))
whereIsLocal streamId = do
  let name = Names.egestLocalProxyName streamId
  isPresentLocally <- Gproc.isRegistered name
  if isPresentLocally
  then pure $ Just name
  else pure $ Nothing




type StartArgs = { mode :: ProxyMode
                 , streamId :: StreamId
                 , forServer :: Server
                 , aggregator :: Server
                 }

startLink :: StartArgs -> Effect StartLinkResult
startLink args@{streamId, forServer} = do
  thisServer <- PoPDefinition.getThisServer
  let
    isLocal = thisServer == forServer
    name = if isLocal
             then
               Names.egestLocalProxyName streamId
             else
               Names.egestRemoteProxyName streamId forServer
  Gen.startLink name (init isLocal args) handleInfo


init :: Boolean -> StartArgs -> Effect State
init isLocal {streamId, forServer, aggregator, mode} = do
  _ <- logInfo "Egest proxy starting" {}
  _ <- case mode of
      New -> do
        let
          payload = { streamId
                    , aggregator
                    } :: CreateEgestPayload

        if isLocal
        then do
          EgestInstanceSup.startEgest payload

        else do
          let
            url = makeUrl forServer EgestE
          void <$> crashIfLeft =<< SpudGun.postJson url payload
      Existing ->
          pure unit
  pure $ { streamId, forServer, mode}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  pure $ CastNoReply state


findEgestForStream :: StreamId -> Effect (Either FailureReason EgestLocation)
findEgestForStream streamId = do

  -- does the stream even exists
  mAggregator <- IntraPoP.whereIsIngestAggregator streamId
  case spy "mAggregator" mAggregator of
    Nothing ->
      pure $ Left NotFound
    Just aggregator -> do
      apiResp <- EgestInstance.addClient streamId
      let pickCandidate = head
      case apiResp of
        Right _ ->
          pure $ Right Local
        Left _ -> do
          proxies <- whereIsRemote streamId
          proxiesAndCapacity <- traverse (\proxy -> do accept <- couldAcceptClient proxy
                                                       pure $ Tuple proxy accept) proxies
          let
            mProxy = pickCandidate $ filterMap (\(Tuple proxy hasCap) -> if hasCap then Just proxy else Nothing) proxiesAndCapacity
          case mProxy of
            Just (proxy :: ServerName State Msg) -> do
              proxyServer <- crashIfLeft =<< getProxyFor proxy
              pure $ Right $ Remote $ extractAddress proxyServer
            Nothing -> do
              mIdleServer <- (map serverLoadToServer) <$> IntraPoP.getIdleServer (const true)
              case mIdleServer of
                Nothing ->
                  pure $ Left NoResource
                Just idleServer -> do
                   let args = { mode : New
                              , streamId
                              , forServer : idleServer
                              , aggregator}
                   _ <- crashIfLeft =<< startLink args
                   findEgestForStream streamId



getProxyFor :: forall a. ServerName State Msg -> Effect (Either a Server)
getProxyFor serverName = Right <$> exposeStateMember serverName _.forServer



couldAcceptClient :: ServerName State Msg -> Effect Boolean
couldAcceptClient sn = pure true



exposeStateMember :: forall a m. ServerName State m -> (State -> a) -> Effect a
exposeStateMember serverName member = Gen.doCall serverName
  \state -> pure $ CallReply (member state) state

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom (show Agent.Egest) : atom "proxy" : nil

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
