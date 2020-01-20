module Quickcheck.TestAgents where

import Prelude

import Data.Generic.Rep (to)
import Data.Newtype (class Newtype)
import Shared.Agent (Agent, agentToStr, strToAgent)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

encodeDecodeIsIdentity :: AgentNT -> Boolean
encodeDecodeIsIdentity (AgentNT agent) = (agentToStr >>> strToAgent) agent == agent

newtype AgentNT = AgentNT Agent
derive instance newtypeFooNT :: Newtype AgentNT _


instance arbitraryFooNT :: Arbitrary AgentNT where
  arbitrary = AgentNT <$> to <$> arbitrary
