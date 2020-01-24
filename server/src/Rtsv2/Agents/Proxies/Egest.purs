module Rtsv2.Agents.Egest.Locator
       ( findEgestForStream
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, filter, head, nil, (:))
import Gproc as Gproc
import Logger (Logger)
import Logger as Logger
import Pinto (ServerName, StartChildResult(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
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
import Shared.Types (EgestLocation(..), FailureReason(..), Server, ServerLoad(..), extractAddress, serverLoadToServer)
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


type StartArgs= { streamId :: StreamId
                , forServer :: Server
                , aggregator :: Server
                }

startLocalOrRemote :: StartArgs -> Effect Unit
startLocalOrRemote  args@{streamId, aggregator, forServer} = do
  thisServer <- PoPDefinition.getThisServer
  let
    isLocal = thisServer == forServer
  if isLocal
  then
    void <$> crashIfLeft =<< startChildToStartLink <$> EgestInstanceSup.startEgest {streamId, aggregator}
  else
    let
      url = makeUrl forServer EgestE
    in
      void <$> crashIfLeft =<< SpudGun.postJson url ({streamId, aggregator} :: CreateEgestPayload)


startChildToStartLink :: StartChildResult -> StartLinkResult
startChildToStartLink (AlreadyStarted pid) =  Right pid
startChildToStartLink (Started pid) = Right pid


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
          allEgest <- IntraPoP.whereIsEgest streamId
          let
            mEgest =  pickCandidate $ filter ((\(ServerLoad sl) -> unwrap sl.load < 50.0)) allEgest
          case mEgest of
            Just egest -> do
              pure $ Right $ Remote $ extractAddress egest
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
                    _ <- startLocalOrRemote args
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
