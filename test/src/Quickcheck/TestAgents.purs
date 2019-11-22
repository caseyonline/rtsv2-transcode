module Quickcheck.TestAgents where

import Prelude
import Test.QuickCheck

import Shared.Agents (Agent(..), agentToStr, strToAgent)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, genericArbitrary)

encodeDecodeIsIdentity :: Agent -> Boolean
encodeDecodeIsIdentity agent = (agentToStr >>> strToAgent) agent == agent
