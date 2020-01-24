module Rtsv2.Agents.Proxies.Egest
       ( createProxyForExisting
       , createProxyForNew
       , whereIsLocal
       , whereIsRemote
       , startLink
       , findEgestForStream
       , ProxyMode
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
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
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Utils (crashIfLeft)
import Shared.Agent as Agent
import Shared.Stream (StreamId(..))
import Shared.Types (EgestLocation(..), FailureReason(..), Server(..), ServerAddress(..), ServerLoad(..), extractAddress, serverLoadToServer)

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


createProxyForExisting :: StreamId -> Server -> Effect StartLinkResult
createProxyForExisting  = startLink Existing

createProxyForNew :: StreamId -> Server -> Effect StartLinkResult
createProxyForNew = startLink New



startLink :: ProxyMode -> StreamId -> Server -> Effect StartLinkResult
startLink mode streamId proxyServer = do
  thisServer <- PoPDefinition.getThisServer
  let
    isLocal = thisServer == proxyServer
    name = if isLocal
             then
               Names.egestLocalProxyName streamId
             else
               Names.egestRemoteProxyName streamId proxyServer
  Gen.startLink name (init streamId proxyServer isLocal mode) handleInfo


init :: StreamId -> Server -> Boolean -> ProxyMode -> Effect State
init streamId forServer isLocal mode = do
  _ <- logInfo "Egest proxy starting" {}
  -- need flag for "should create" - then create one if you think you should

  -- if local - then create and monitor
  -- if remote post and tell IntraPoP you want egestAvailable notifications

       -- let
       --   url = makeUrl idleServer EgestE
       --   payload = { streamId
       --                    , aggregator
       --                    } :: CreateEgestPayload

                -- if extractAddress idleServer == thisAddress
                -- then do
                --   _ <- EgestInstanceSup.maybeStartAndAddClient payload
                --   pure $ Right Local
                -- else do
                --   let
                --     addr = extractAddress idleServer
                --   _ <- crashIfLeft =<< SpudGun.postJson url payload
                --   pure $ Right $ Remote addr

  pure $ { streamId, forServer, mode}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  pure $ CastNoReply state


findEgestForStream :: StreamId -> Effect (Either FailureReason EgestLocation)
findEgestForStream streamId = do
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
          mIdleServer  <- (map serverLoadToServer) <$> IntraPoP.getIdleServer (const true)
          case mIdleServer of
            Nothing ->
              pure $ Left NoResource
            Just idleServer -> do
               _ <- crashIfLeft =<< createProxyForNew streamId idleServer
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
