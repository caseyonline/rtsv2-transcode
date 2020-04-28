module Rtsv2.Agents.Locator.Egest
       ( findEgest
       )
       where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, filter, head, nil, (:))
import Logger (Logger)
import Logger as Logger
import Pinto (okAlreadyStarted)
import Rtsv2.Agents.EgestInstance (CreateEgestPayload)
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator.Types (FailureReason(..), LocalOrRemote(..), LocationResp)
import Rtsv2.Config (LoadConfig)
import Rtsv2.Load as Load
import Rtsv2.LoadTypes (LoadCheckResult(..))
import Rtsv2.Utils (crashIfLeft, noprocToMaybe)
import Shared.Rtsv2.Agent (SlotCharacteristics)
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Rtsv2.Stream (AggregatorKey(..), EgestKey(..), SlotId, SlotRole)
import Shared.Rtsv2.Types (Server, serverLoadToServer)
import SpudGun as SpudGun

type StartArgs = { slotId :: SlotId
                 , forServer :: Server
                 , aggregator :: Server
                 }

findEgest :: SlotId -> SlotRole -> LoadConfig -> Server -> Effect LocationResp
findEgest slotId slotRole loadConfig thisServer = do
  mIngestAggregator <- IntraPoP.whereIsIngestAggregatorWithPayload (AggregatorKey slotId slotRole)
  case mIngestAggregator of
    Nothing ->  pure $ Left NotFound
    Just {payload, server: aggregator} -> findEgest' slotId slotRole loadConfig thisServer payload aggregator

findEgest' :: SlotId -> SlotRole -> LoadConfig -> Server -> SlotCharacteristics -> Server -> Effect LocationResp
findEgest' slotId slotRole loadConfig thisServer slotCharacteristics aggregator = runExceptT
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
      eResourceResp <- getIdle
      case eResourceResp of
        Left error -> pure $ Left error
        Right localOrRemote -> do
          startLocalOrRemote localOrRemote
          findEgest' slotId slotRole loadConfig thisServer slotCharacteristics aggregator

    getIdle = (lmap (const NoResource)) <$> IntraPoP.getIdleServer capacityForEgest

    startLocalOrRemote (Local _) =
      okAlreadyStarted =<< EgestInstanceSup.startEgest {slotId, slotRole, aggregator, slotCharacteristics}
    startLocalOrRemote (Remote remote) =
      void <$> crashIfLeft =<< SpudGun.postJson (makeUrl remote EgestE) ({slotId, slotRole, aggregator, slotCharacteristics} :: CreateEgestPayload)

    egestKey = (EgestKey slotId slotRole)
    pickCandidate = head

    capacityForClient = ((==) Green) <<< Load.hasCapacityForEgestClient slotCharacteristics loadConfig

    capacityForEgest = Load.hasCapacityForEgestInstance slotCharacteristics loadConfig

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom (show Agent.Egest) : atom "locator" : nil

logInfo :: forall a. Logger (Record a)
logInfo = domainLog Logger.info

logWarning :: forall a. Logger (Record a)
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: Record a} -> Logger (Record a)
domainLog = Logger.doLog domains
