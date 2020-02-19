module Rtsv2.Handler.IntraPoP
  ( leader
  , publicState
  , testHelper
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Erl.Data.List (List)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.IntraPoP as IntraPoPAgent
import Shared.Types (extractAddress)
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GenericStetsonGet, GenericStetsonHandler, genericGet, genericGetText, genericPost)

--TODO - use genericGet
--leader:: StetsonHandler String
leader :: GenericStetsonGet String
leader = genericGetText do
  mLeader <- IntraPoPAgent.getCurrentTransPoPLeader
  pure $ fromMaybe "" (unwrap <<< extractAddress <$> mLeader)

testHelper :: GenericStetsonHandler IntraPoP.TestHelperPayload
testHelper =  genericPost IntraPoP.testHelper

publicState :: GenericStetsonGet (PublicState.IntraPoP List)
publicState = genericGet IntraPoP.getPublicState
