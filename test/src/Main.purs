module Main where

import Prelude

import Cases.BrowserDataMsging (browserDataMsging)
import Cases.BrowserIngestTest (browserIngestTest)
import Cases.Canary (canaryTests)
import Cases.Drain (drainTests)
import Cases.Ingest (ingestTests)
import Cases.IngestEgest (ingestEgestTests)
import Cases.Load (loadTests)
import Cases.Metrics (metricsTests)
import Cases.Resilience (resilienceTests)
import Cases.RtmpEgest (rtmpEgestTests)
import Cases.Startup (startupTests)
import Cases.StreamDiscovery (streamDiscoveryTests)
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
    startupTests      -- 1
    ingestTests       -- 2
    ingestEgestTests  -- 3
    resilienceTests   -- 4
    browserIngestTest -- 5
    browserDataMsging -- 6
    loadTests         -- 7
    canaryTests       -- 8
    drainTests        -- 9
    metricsTests      -- 10
    streamDiscoveryTests -- 11
    rtmpEgestTests -- 12
    describe "Cleanup" do
      after_ F.stopSession do
        it "final cleanup" do
          pure unit
  where
    testConfig = { slow: Milliseconds 5000.0, timeout: Just (Milliseconds 60000.0), exit: false }
