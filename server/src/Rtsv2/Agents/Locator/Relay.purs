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
import Rtsv2.Agents.StreamRelayInstanceSup as StreamRelayInstanceSup
import Rtsv2.Agents.StreamRelayTypes (CreateRelayPayload)
import Shared.Agent as Agent
import Shared.Stream (RelayKey(..))
import Shared.Types (Server, ServerLoad(..))




findOrStart :: CreateRelayPayload -> Effect (ResourceResp Server)
findOrStart =
  Locator.findOrStart { findFun : IntraPoP.whereIsStreamRelay <<< payloadToRelayKey
                      , handlerCreationPredicate : hasCapcityForRelay
                      , startLocalFun : startLocalRelay
                      , logWarning
                      }
  where
    payloadToRelayKey payload = RelayKey payload.streamId payload.streamRole
    hasCapcityForRelay (ServerLoad sl) =  unwrap sl.load < 50.0
    startLocalRelay payload = do
      _ <- StreamRelayInstanceSup.startRelay { streamId: payload.streamId, streamRole: payload.streamRole, aggregator: payload.aggregator}
      pure unit





-- findRelayAndRegisterForChain :: RegisterRelayChainPayload -> Effect LocationResp
-- findRelayAndRegisterForChain =
--   findAndRegister { registerFun : registerRelayChain
--                   , findFun : IntraPoP.whereIsStreamRelay <<< _.streamId
--                   , handlerCreationPredicate : hasCapcityForRelay
--                   , startLocalFun : startLocalRelay
--                   , startRemoteFun : startRemoteRelay
--                   , logWarning
--                   }
--   where
--    hasCapcityForRelay (ServerLoad sl) =  unwrap sl.load < 50.0
--    startLocalRelay payload = do
--      _ <- StreamRelayInstanceSup.startRelay { streamId: payload.streamId, aggregatorPoP : payload.aggregatorPoP}
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
-- startLocalOrRemote  (Local _) streamId aggregator = do
--   void <$> crashIfLeft =<< startChildToStartLink <$> EgestInstanceSup.startEgest {streamId, aggregator}
-- startLocalOrRemote  (Remote remote) streamId aggregator = do
--   let
--     url = makeUrl remote EgestE
--   void <$> crashIfLeft =<< SpudGun.postJson url ({streamId, aggregator} :: CreateEgestPayload)

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
