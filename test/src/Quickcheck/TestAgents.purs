module Quickcheck.TestAgents where

import Prelude
import Test.QuickCheck

import Data.Generic.Rep (class Generic, to)
import Data.Newtype (class Newtype, wrap)
import Shared.Agent (Agent(..), agentToStr, strToAgent)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, genericArbitrary)
import Test.QuickCheck.Gen (Gen)

encodeDecodeIsIdentity :: AgentNT -> Boolean
encodeDecodeIsIdentity (AgentNT agent) = (agentToStr >>> strToAgent) agent == agent

newtype AgentNT = AgentNT Agent
derive instance newtypeFooNT :: Newtype AgentNT _


instance arbitraryFooNT :: Arbitrary AgentNT where
  arbitrary = AgentNT <$> to <$> arbitrary
