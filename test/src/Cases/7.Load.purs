module Cases.Load where

import Prelude

import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Helpers.Types (Node)
import Helpers.Assert as A
import Helpers.Env as E
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log (as, as')
import Shared.Rtsv2.Types (CurrentLoad(..), CanaryState(..))
import Test.Spec (SpecT, after_, before_, describe, it)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
loadTests :: forall m. Monad m => SpecT Aff Unit m Unit
loadTests = do
  describe "7 load" do
    before_ (E.lookupPuppeteerEnv *> F.startSession nodes *> F.launch nodes) do
      after_ F.stopSession do
        loadTest1

-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------
nodes :: Array Node
nodes = [E.p1n1]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------
loadTest1 :: forall m. Monad m => SpecT Aff Unit m Unit
loadTest1 =
  it "7.1 Launch ingest, launch egest, observe load decay on egest node" do
    HTTP.ingestStart E.p1n1 Live E.shortName1 E.lowStreamName >>= A.assertStatusCode 200 >>= as  "create ingest"
    E.waitForAsyncProfileStart                                            >>= as' "wait for async start of profile"
    HTTP.getLoad E.p1n1                        >>= A.assertStatusCode 200
                                               >>= A.assertLoadEqual (CurrentLoad {cpu: wrap 0.0, network: wrap 0})
                                                                          >>= as "check load is zero"
    HTTP.clientStart E.p1n1 Live E.slot1       >>= A.assertStatusCode 204 >>= as  "start egest"
    E.waitForAsyncProfileStart                                            >>= as' "wait for async start of profile"
    HTTP.getLoad E.p1n1                        >>= A.assertStatusCode 200
                                               >>= A.assertLoadGreaterThan (CurrentLoad {cpu: wrap 0.3, network: wrap 100000})
                                                                          >>= as "check load is high"
    E.waitForHalfLoadDecay                                                >>= as' "wait for decay"
    HTTP.getLoad E.p1n1                        >>= A.assertStatusCode 200
                                               >>= A.assertLoadLessThan (CurrentLoad {cpu: wrap 0.3, network: wrap 100000})
                                                                          >>= as "check load is decaying"
    E.waitForHalfLoadDecay                                                >>= as' "wait for decay"
    E.waitForLoadTick                                                     >>= as' "and make sure tick has happened"
    HTTP.getLoad E.p1n1                        >>= A.assertStatusCode 200
                                               >>= A.assertLoadEqual (CurrentLoad {cpu: wrap 0.0, network: wrap 0})
                                                                          >>= as "check load is back to zero"
