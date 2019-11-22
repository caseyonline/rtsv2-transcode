module Test where

import Effect (Effect)
import Prelude (Unit)
import Quickcheck.TestAgents as TestAgents
import Test.QuickCheck (quickCheck)

main :: Effect Unit
main = quickCheck TestAgents.encodeDecodeIsIdentity
