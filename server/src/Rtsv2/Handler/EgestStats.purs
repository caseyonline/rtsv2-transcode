module Rtsv2.Handler.EgestStats
       (
         stats
       ) where

import Prelude

import Rtsv2.Agents.EgestInstance as EgestInstance
import Shared.Stream (EgestKey(..), StreamId)
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GenericStetsonGet, genericGet)

stats :: StreamId -> GenericStetsonGet PublicState.Egest
stats = genericGet <<< EgestInstance.currentStats <<< EgestKey
