module Cases.Startup where

import Prelude

import Effect.Aff (Aff)
import Data.Array as Array
import Data.List as List
import Data.Traversable (traverse)
import Shared.Rtsv2.JsonLd as JsonLd
import Helpers.Assert as A
import Helpers.Log as L
import Helpers.Env as Env
import Helpers.Functions as F
import Test.Spec (SpecT, after_, before_, describe, it)


startupTests :: forall m. Monad m => SpecT Aff Unit m Unit
startupTests = do
  describe "1 Startup tests" do
    let p1Nodes = [Env.p1n1, Env.p1n2, Env.p1n3]
        nodes = p1Nodes
    before_ (F.startSession nodes *> F.launch nodes) do
        after_ F.stopSession do
          it "1.1 Nodes all come up and agree on who the leader is" do
            states <- traverse F.forceGetState (Array.toUnfoldable p1Nodes) :: Aff (List.List (JsonLd.IntraPoPState Array))
            let leaders = JsonLd.unwrapNode <$> List.catMaybes (_.currentTransPoPLeader <$> states)
            A.assertSame leaders >>= L.as "All nodes agree on leader and other intial state"
