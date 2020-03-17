module Rtsv2.Agents.Locator.Relay
       ( findOrStart
       )
       where

import Prelude

import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Logger (Logger)
import Logger as Logger
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator (findOrStart) as Locator
import Rtsv2.Agents.Locator.Types (ResourceResp)
import Rtsv2.Agents.StreamRelaySup as StreamRelaySup
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload)
import Shared.Agent as Agent
import Shared.Stream (RelayKey(..))
import Shared.Types (Server, ServerLoad(..))

findOrStart :: CreateRelayPayload -> Effect (ResourceResp Server)
findOrStart =
  Locator.findOrStart { findFun : IntraPoP.whereIsStreamRelay <<< payloadToRelayKey
                      , handlerCreationPredicate : hasCapacityForRelay
                      , startLocalFun : startLocalRelay
                      , logWarning
                      }
  where
    payloadToRelayKey payload = RelayKey payload.slotId payload.slotRole
    hasCapacityForRelay (ServerLoad sl) =  unwrap sl.load < 50.0
    startLocalRelay payload = do
      _ <- StreamRelaySup.startRelay { slotId: payload.slotId, slotRole: payload.slotRole, aggregator: payload.aggregator}
      pure unit

-- findRelayAndRegisterForChain :: RegisterRelayChainPayload -> Effect LocationResp
-- findRelayAndRegisterForChain =
--   findAndRegister { registerFun : registerRelayChain
--                   , findFun : IntraPoP.whereIsStreamRelay <<< _.slotId
--                   , handlerCreationPredicate : hasCapacityForRelay
--                   , startLocalFun : startLocalRelay
--                   , startRemoteFun : startRemoteRelay
--                   , logWarning
--                   }
--   where
--    hasCapacityForRelay (ServerLoad sl) =  unwrap sl.load < 50.0
--    startLocalRelay payload = do
--      _ <- StreamRelayInstanceSup.startRelay { slotId: payload.slotId, aggregatorPoP : payload.aggregatorPoP}
--      pure unit
--    startRemoteRelay sl payload = do
--      let
--        url = makeUrl sl RelayE
--      resp <- SpudGun.postJson url payload
--      pure unit


--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------
-- startLocalOrRemote :: (LocalOrRemote ServerLoad) -> SlotId -> Server -> Effect Unit
-- startLocalOrRemote  (Local _) slotId aggregator = do
--   void <$> crashIfLeft =<< startChildToStartLink <$> EgestInstanceSup.startEgest {slotId, aggregator}
-- startLocalOrRemote  (Remote remote) slotId aggregator = do
--   let
--     url = makeUrl remote EgestE
--   void <$> crashIfLeft =<< SpudGun.postJson url ({slotId, aggregator} :: CreateEgestPayload)

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom (show Agent.StreamRelay) : atom "locator" : nil

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
