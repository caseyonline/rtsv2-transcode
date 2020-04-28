module Rtsv2.Agents.Locator.Relay
       ( findOrStart
       )
       where

import Prelude

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
import Rtsv2.Config as Config
import Rtsv2.Load as Load
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.Stream (RelayKey(..))
import Shared.Rtsv2.Types (Server)

findOrStart :: Config.LoadConfig -> CreateRelayPayload -> Effect (ResourceResp Server)
findOrStart loadConfig payload@{slotCharacteristics} =
  Locator.findOrStart { findFun : IntraPoP.whereIsStreamRelay <<< payloadToRelayKey
                      , handlerCreationPredicate : Load.hasCapacityForStreamRelay slotCharacteristics loadConfig
                      , startLocalFun : startLocalRelay
                      , logWarning
                      } payload
  where
    payloadToRelayKey payload = RelayKey payload.slotId payload.slotRole
    startLocalRelay payload = do
      _ <- StreamRelaySup.startRelay { slotId: payload.slotId, slotRole: payload.slotRole, aggregator: payload.aggregator, slotCharacteristics}
      pure unit

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = atom (show Agent.StreamRelay) : atom "locator" : nil

logInfo :: forall a. Logger (Record a)
logInfo = domainLog Logger.info

logWarning :: forall a. Logger (Record a)
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: Record a} -> Logger (Record a)
domainLog = Logger.doLog domains
