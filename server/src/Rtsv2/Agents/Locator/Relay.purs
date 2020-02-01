module Rtsv2.Agents.Locator.Relay
       ( findOrStart
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Logger (Logger)
import Logger as Logger
import Pinto (StartChildResult(..), StartLinkResult)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator (findOrStart) as Locator
import Rtsv2.Agents.Locator.Types (ResourceResp)
import Rtsv2.Agents.StreamRelay.Types (CreateRelayPayload)
import Rtsv2.Agents.StreamRelay.InstanceSup as StreamRelayInstanceSup
import Shared.Agent as Agent
import Shared.Types (Server, ServerLoad(..))




findOrStart :: CreateRelayPayload -> Effect (ResourceResp Server)
findOrStart =
  Locator.findOrStart { findFun : IntraPoP.whereIsStreamRelay <<< _.streamId
                      , handlerCreationPredicate : hasCapcityForRelay
                      , startLocalFun : startLocalRelay
                      , logWarning
                      }
  where
    hasCapcityForRelay (ServerLoad sl) =  unwrap sl.load < 50.0
    startLocalRelay payload = do
      _ <- StreamRelayInstanceSup.startRelay { streamId: payload.streamId, aggregatorPoP : payload.aggregatorPoP}
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
-- startLocalOrRemote :: (LocalOrRemote ServerLoad) -> StreamId -> Server -> Effect Unit
-- startLocalOrRemote  (Local _) streamId aggregator = do
--   void <$> crashIfLeft =<< startChildToStartLink <$> EgestInstanceSup.startEgest {streamId, aggregator}
-- startLocalOrRemote  (Remote remote) streamId aggregator = do
--   let
--     url = makeUrl remote EgestE
--   void <$> crashIfLeft =<< SpudGun.postJson url ({streamId, aggregator} :: CreateEgestPayload)


startChildToStartLink :: StartChildResult -> StartLinkResult
startChildToStartLink (AlreadyStarted pid) =  Right pid
startChildToStartLink (Started pid) = Right pid

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
