module Rtsv2.Agents.Locator.Relay
       ( findRelayAndRegisterForChain
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Logger (Logger)
import Logger as Logger
import Pinto (StartChildResult(..), StartLinkResult)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.Locator (findAndRegister)
import Rtsv2.Agents.Locator.Types (LocalOrRemote, LocationResp, ServerSelectionPredicate)
import Rtsv2.Agents.StreamRelayInstance (RegisterRelayChainPayload, registerRelayChain)
import Rtsv2.Agents.StreamRelayInstanceSup as StreamRelayInstanceSup
import Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
import Shared.Agent as Agent
import Shared.Types (Server, ServerLoad(..))
import SpudGun as SpudGun



type FindAndRegisterConfig payload
  = { registerFun :: payload -> Effect Unit
    , findFun :: (payload -> Effect (Maybe (LocalOrRemote Server)))
    , handlerCreationPredicate :: ServerSelectionPredicate
    , startLocalFun :: payload -> Effect Unit
    , startRemoteFun :: ServerLoad -> payload -> Effect Unit
    , logWarning :: forall a. Logger a
    }



findRelayAndRegisterForChain :: RegisterRelayChainPayload -> Effect LocationResp
findRelayAndRegisterForChain =
  findAndRegister { registerFun : registerRelayChain
                  , findFun : IntraPoP.whereIsStreamRelay <<< _.streamId
                  , handlerCreationPredicate : hasCapcityForRelay
                  , startLocalFun : startLocalRelay
                  , startRemoteFun : startRemoteRelay
                  , logWarning
                  }
  where
   hasCapcityForRelay (ServerLoad sl) =  unwrap sl.load < 50.0
   startLocalRelay payload = do
     _ <- StreamRelayInstanceSup.startRelay { streamId: payload.streamId, aggregatorPoP : payload.aggregatorPoP}
     pure unit
   startRemoteRelay sl payload = do
     let
       url = makeUrl sl RelayE
     resp <- SpudGun.postJson url payload
     pure unit


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
