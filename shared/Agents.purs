module Shared.Agents (
                Agent(..)
              , strToAgent
              , agentToStr
              ) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (class Eq, class Show, show, (<>))

--import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data Agent = EdgeAgent
           | IngestAgent
           | StreamRelayAgent
           | IntraPoPAgent
           | TransPoPAgent

derive instance genericAgent :: Generic Agent _

instance eqAgent :: Eq Agent where
  eq = genericEq

instance showAgent :: Show Agent where
  show = genericShow

--instance arbitraryAgent ∷ Arbitrary Agent where
--  arbitrary = genericArbitrary

strToAgent :: String -> Agent
strToAgent "EdgeAgent" = EdgeAgent
strToAgent "IngestAgent" = IngestAgent
strToAgent "StreamRelayAgent" = StreamRelayAgent
strToAgent "IntraPoPAgent" = IntraPoPAgent
strToAgent "TransPoPAgent" = TransPoPAgent
strToAgent unknown = unsafeCrashWith ("Unknown Agent" <> unknown)

agentToStr :: Agent -> String
agentToStr = show
