module Main where

import Prelude

import Cases.WebRTCTest (webRTCTest)
import Cases.Startup (startupTests)
import Cases.Ingest (ingestTests)
import Cases.IngestEgest (ingestEgestTests)
import Cases.Resilience (resilienceTests)
import Cases.Load (loadTests)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Helpers.Functions as F
import Test.Spec (after_, describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT)

main :: Effect Unit
main =
  launchAff_ $ un Identity $ runSpecT testConfig [consoleReporter] do
    startupTests -- 1
    ingestTests -- 2
    ingestEgestTests -- 3
    resilienceTests -- 4
    webRTCTest -- 5
    loadTests -- 6
    describe "Cleanup" do
      after_ F.stopSession do
        it "final cleanup" do
          pure unit
  where
    testConfig = { slow: Milliseconds 5000.0, timeout: Just (Milliseconds 25000.0), exit: false }
