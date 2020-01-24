module Rtsv2.Agents.Egest.Locator
       ( proxiesFor
       , startOrProxy
       , startLink
       , findEgestForStream
       , StartArgs(..)
       , NewArgs_
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
import Pinto (ServerName, StartChildResult(..), StartLinkResult)
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

type State = { streamId :: StreamId
             , forServer :: Server
             }

type Msg = Unit

proxiesFor :: StreamId -> Effect (List (ServerName State Msg))
proxiesFor streamId = do
  Gproc.match $ Names.egestProxyMatch streamId

serverName :: StreamId -> Server -> ServerName State Msg
serverName = Names.egestProxyName


type NewArgs_ = { streamId :: StreamId
      , forServer :: Server
      , aggregator :: Server
      }

data StartArgs =
  New NewArgs_
  | Existing { streamId :: StreamId
             , forServer :: Server
             }

startLink :: StartArgs -> Effect StartLinkResult
startLink args@(New {streamId, forServer}) = Gen.startLink (serverName streamId forServer) (init args) handleInfo
startLink args@(Existing {streamId, forServer}) = Gen.startLink (serverName streamId forServer) (init args) handleInfo


startOrProxy :: NewArgs_ -> Effect StartLinkResult
startOrProxy  args@{streamId, aggregator, forServer} = do
  thisServer <- PoPDefinition.getThisServer
  let
    isLocal = thisServer == forServer
  if isLocal
  then do
    startChildToStartLink <$> EgestInstanceSup.startEgest {streamId, aggregator}
  else
    -- TODO - via a supervisor?
    startLink (New args)


-- TODO - this probably should not live here...
-- TODO - is alreadyStarted an error here?
startChildToStartLink :: StartChildResult -> StartLinkResult
startChildToStartLink (AlreadyStarted pid) =  Right pid
startChildToStartLink (Started pid) = Right pid


init :: StartArgs -> Effect State
init (New payload@{streamId, forServer, aggregator}) = do
  _ <- logInfo "Egest proxy starting for new egest" payload
  let
    url = makeUrl forServer EgestE
  _ <- crashIfLeft =<< SpudGun.postJson url ({streamId, aggregator} :: CreateEgestPayload)
  pure $ { streamId, forServer}


init (Existing payload@{streamId, forServer}) = do
  _ <- logInfo "Egest proxy starting for existing egest" payload
  pure $ { streamId, forServer}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  pure $ CastNoReply state


findEgestForStream :: StreamId -> Effect (Either FailureReason EgestLocation)
findEgestForStream streamId = do

  apiResp <- EgestInstance.addClient streamId
  case apiResp of
    Right _ ->
      pure $ Right Local
    Left _ -> do
      -- does the stream even exists
      mAggregator <- IntraPoP.whereIsIngestAggregator streamId
      case spy "mAggregator" mAggregator of
        Nothing ->
          pure $ Left NotFound

        Just aggregator -> do
          proxies <- proxiesFor streamId
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
                Just idleServer ->
                  let args = { streamId
                             , forServer : idleServer
                             , aggregator
                             }
                  in do
                    _ <- crashIfLeft =<< startOrProxy args
                    findEgestForStream streamId
  where pickCandidate = head


getProxyFor :: forall a. ServerName State Msg -> Effect (Either a Server)
getProxyFor sn = Right <$> exposeStateMember sn _.forServer


couldAcceptClient :: ServerName State Msg -> Effect Boolean
couldAcceptClient sn = pure true



exposeStateMember :: forall a m. ServerName State m -> (State -> a) -> Effect a
exposeStateMember sn member = Gen.doCall sn
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
