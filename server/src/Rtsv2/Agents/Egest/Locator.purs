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
import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Names as Names
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Rtsv2.Utils (crashIfLeft)
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (FailureReason(..), LocalOrRemote(..), Server, ServerLoad(..), serverLoadToServer)
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


type StartArgs = { streamId :: StreamId
                 , forServer :: Server
                 , aggregator :: Server
                 }

startLocalOrRemote :: (LocalOrRemote ServerLoad) -> StreamId -> Server -> Effect Unit
startLocalOrRemote  (Local _) streamId aggregator = do
  void <$> crashIfLeft =<< startChildToStartLink <$> EgestInstanceSup.startEgest {streamId, aggregator}
startLocalOrRemote  (Remote remote) streamId aggregator = do
  let
    url = makeUrl remote EgestE
  void <$> crashIfLeft =<< SpudGun.postJson url ({streamId, aggregator} :: CreateEgestPayload)


startChildToStartLink :: StartChildResult -> StartLinkResult
startChildToStartLink (AlreadyStarted pid) =  Right pid
startChildToStartLink (Started pid) = Right pid


findEgestForStream :: StreamId -> Server ->Effect (Either FailureReason (LocalOrRemote Server))
findEgestForStream streamId thisServer = do

  apiResp <- EgestInstance.addClient streamId
  case spy "apiResp" apiResp of
    Right _ ->
      pure $ Right $ Local thisServer
    Left _ -> do
      -- does the stream even exists
      mAggregator <- IntraPoP.whereIsIngestAggregator streamId
      case spy "mAggregator" mAggregator of
        Nothing ->
          pure $ Left NotFound
        Just aggregator -> do
          allEgest <- IntraPoP.whereIsEgest streamId
          let
            mEgest =  pickCandidate $ filter capcityForClient allEgest
          case mEgest of
            Just egest -> do
              pure $ Right $ Remote $ serverLoadToServer egest
            Nothing -> do
              resourceResp <- IntraPoP.getIdleServer capcityForEgest
              case resourceResp of
                Left _ ->
                  pure $ Left NoResource
                Right localOrRemote -> do
                  startLocalOrRemote localOrRemote streamId aggregator
                  findEgestForStream streamId thisServer
  where
   pickCandidate = head
   capcityForClient (ServerLoad sl) =  unwrap sl.load < 90.0
   capcityForEgest (ServerLoad sl) =  unwrap sl.load < 50.0




couldAcceptClient :: ServerName State Msg -> Effect Boolean
couldAcceptClient sn = pure true


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
