module Rtsv2.Handler.IntraPoP
  ( leader
  , publicState
  , testHelper
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Erl.Data.List (List)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.IntraPoP as IntraPoPAgent
import Shared.Types (extractAddress)
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GetResponse, GenericStetsonHandler, jsonResponse, textResponse, genericPost)

leader :: GetResponse String
leader = textResponse do
  mLeader <- IntraPoPAgent.getCurrentTransPoPLeader
  pure $ (unwrap <<< extractAddress <$> mLeader)

testHelper :: GenericStetsonHandler IntraPoP.TestHelperPayload
testHelper =  genericPost IntraPoP.testHelper

publicState :: GetResponse (PublicState.IntraPoP List)
publicState = jsonResponse $ Just <$> IntraPoP.getPublicState
