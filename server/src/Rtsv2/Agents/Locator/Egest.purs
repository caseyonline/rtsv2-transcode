module Rtsv2.Agents.Locator.Egest
       ( findEgest
       )
       where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
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
import Rtsv2.Utils (crashIfLeft, noprocToMaybe)
import Shared.Agent as Agent
import Shared.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Stream (AggregatorKey(..), EgestKey(..), SlotId, SlotRole(..))
import Shared.Types (Server, ServerLoad(..), serverLoadToServer)
import SpudGun as SpudGun

type StartArgs = { slotId :: SlotId
                 , forServer :: Server
                 , aggregator :: Server
                 }

findEgest :: SlotId -> Server -> Effect LocationResp
findEgest slotId thisServer = runExceptT
  $ ExceptT getLocal
  <|> ExceptT getRemote
  <|> ExceptT createResourceAndRecurse
  where
    getLocal = do
      local <- noprocToMaybe $ EgestInstance.pendingClient egestKey
      pure $ note NotFound $ (const (Local thisServer)) <$> local

    getRemote = do
      mEgest <- pickCandidate <$> filter capacityForClient <$> IntraPoP.whereIsEgest egestKey
      pure $ note NotFound $ (Remote <<< serverLoadToServer) <$> mEgest

    createResourceAndRecurse = do
      eAggregator <- findAggregator
      eResourceResp <- getIdle
      let
        _ = lift2 startLocalOrRemote eAggregator eResourceResp
      findEgest' slotId thisServer

    findAggregator = (note NotFound) <$> IntraPoP.whereIsIngestAggregator (AggregatorKey slotId Primary)

    getIdle = (lmap (const NoResource)) <$> IntraPoP.getIdleServer capacityForEgest

    startLocalOrRemote aggregator (Local _) =
      void <$> okAlreadyStarted =<<  EgestInstanceSup.startEgest {slotId, aggregator}
    startLocalOrRemote aggregator (Remote remote) =
      void <$> crashIfLeft =<< SpudGun.postJson (makeUrl remote EgestE) ({slotId, aggregator} :: CreateEgestPayload)

    egestKey = (EgestKey slotId)
    pickCandidate = head
    capacityForClient (ServerLoad sl) =  unwrap sl.load < 90.0
    capacityForEgest (ServerLoad sl) =  unwrap sl.load < 50.0

findEgest' :: SlotId -> Server -> Effect LocationResp
findEgest' slotId thisServer = do
  let
    egestKey = (EgestKey slotId)
  apiResp <- noprocToMaybe $ EgestInstance.pendingClient egestKey
  case apiResp of
    Just _ ->
      pure $ Right $ Local thisServer
    Nothing -> do
      -- does the stream even exists
      -- TODO - Primary and Backup
      mAggregator <- IntraPoP.whereIsIngestAggregator (AggregatorKey slotId Primary)
      case spy "mAggregator" mAggregator of
        Nothing ->
          pure $ Left NotFound
        Just aggregator -> do
          allEgest <- IntraPoP.whereIsEgest egestKey
          let
            mEgest =  pickCandidate $ filter capacityForClient allEgest
          case mEgest of
            Just egest -> do
              pure $ Right $ Remote $ serverLoadToServer egest
            Nothing -> do
              -- TODO use launchLocalOrRemoteGeneric
              resourceResp <- IntraPoP.getIdleServer capacityForEgest
              case resourceResp of
                Left _ ->
                  pure $ Left NoResource
                Right localOrRemote -> do
                  startLocalOrRemote localOrRemote aggregator
                  findEgest slotId thisServer
  where
   pickCandidate = head
   capacityForClient (ServerLoad sl) =  unwrap sl.load < 90.0
   capacityForEgest (ServerLoad sl) =  unwrap sl.load < 50.0
   startLocalOrRemote :: (LocalOrRemote ServerLoad) -> Server -> Effect Unit
   startLocalOrRemote  (Local _) aggregator = do
     void <$> okAlreadyStarted =<<  EgestInstanceSup.startEgest {slotId, aggregator}
   startLocalOrRemote  (Remote remote) aggregator = do
     let
       url = makeUrl remote EgestE
     void <$> crashIfLeft =<< SpudGun.postJson url ({slotId, aggregator} :: CreateEgestPayload)


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
