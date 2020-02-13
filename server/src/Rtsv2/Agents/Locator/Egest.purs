module Rtsv2.Agents.Locator.Egest
       ( findEgestAndRegister
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, filter, head, nil, (:))
import Logger (Logger, spy)
import Logger as Logger
import Pinto (okAlreadyStarted)
import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator.Types (FailureReason(..), LocalOrRemote(..), LocationResp)
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Rtsv2.Utils (crashIfLeft, noprocToMaybe)
import Shared.Agent as Agent
import Shared.Stream (AggregatorKey(..), EgestKey(..), StreamId, StreamRole(..))
import Shared.Types (PoPName, Server, ServerLoad(..), extractPoP, serverLoadToServer)
import SpudGun as SpudGun


type StartArgs = { streamId :: StreamId
                 , forServer :: Server
                 , aggregator :: Server
                 }

findEgestAndRegister :: StreamId -> Server -> Effect LocationResp
findEgestAndRegister streamId thisServer = do
  let
    egestKey = (EgestKey streamId)
  apiResp <- noprocToMaybe $ EgestInstance.addClient egestKey
  case spy "apiResp" apiResp of
    Just _ ->
      pure $ Right $ Local thisServer
    Nothing -> do
      -- does the stream even exists
      -- TODO - Primary and Backup
      mAggregator <- IntraPoP.whereIsIngestAggregator (AggregatorKey streamId Primary)
      case spy "mAggregator" mAggregator of
        Nothing ->
          pure $ Left NotFound
        Just aggregator -> do
          allEgest <- IntraPoP.whereIsEgest egestKey
          let
            _ = spy "allEgest" allEgest
            mEgest =  pickCandidate $ filter capcityForClient allEgest
            _ = spy "mEgest" mEgest
          case mEgest of
            Just egest -> do
              pure $ Right $ Remote $ serverLoadToServer egest
            Nothing -> do
              -- TODO use launchLocalOrRemoteGeneric
              resourceResp <- IntraPoP.getIdleServer capcityForEgest
              case resourceResp of
                Left _ ->
                  pure $ Left NoResource
                Right localOrRemote -> do
                  startLocalOrRemote localOrRemote (extractPoP aggregator)
                  findEgestAndRegister streamId thisServer
  where
   pickCandidate = head
   capcityForClient (ServerLoad sl) =  unwrap sl.load < 90.0
   capcityForEgest (ServerLoad sl) =  unwrap sl.load < 50.0
   startLocalOrRemote :: (LocalOrRemote ServerLoad) -> PoPName -> Effect Unit
   startLocalOrRemote  (Local _) aggregatorPoP = do
     void <$> okAlreadyStarted =<<  EgestInstanceSup.startEgest {streamId, aggregatorPoP}
   startLocalOrRemote  (Remote remote) aggregatorPoP = do
     let
       url = makeUrl remote EgestE
     void <$> crashIfLeft =<< SpudGun.postJson url ({streamId, aggregatorPoP} :: CreateEgestPayload)


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom (show Agent.Egest) : atom "locator" : nil

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
