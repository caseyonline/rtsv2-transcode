module Rtsv2.Handler.IntraPoP
  ( leader
  , publicState
  , testHelper
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Erl.Data.List (List)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.IntraPoP as IntraPoPAgent
import Shared.Types (extractAddress)
import Shared.Types.Agent.State as PublicState
import StetsonHelper (GetHandler, PostHandler, jsonResponse, processPostPayload, textResponse)

leader :: GetHandler String
leader = textResponse do
  mLeader <- IntraPoPAgent.getCurrentTransPoPLeader
  pure $ (unwrap <<< extractAddress <$> mLeader)

testHelper :: PostHandler IntraPoP.TestHelperPayload
testHelper =  processPostPayload IntraPoP.testHelper

publicState :: GetHandler (PublicState.IntraPoP List)
publicState = jsonResponse $ Just <$> IntraPoP.getPublicState
