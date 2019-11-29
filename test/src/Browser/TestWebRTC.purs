module TestWebRTC where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import RTCPeerConnection (getVideoStats)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (Config, runSpecT)
import Test.Unit.Assert as Assert
import Toppokki as T

testConfig :: Config
testConfig = { slow: (Milliseconds 500.00), timeout: Just (Milliseconds 10000.00), exit: true }

launchArgs :: Array String
launchArgs = [ "--vmodule=*/webrtc/*=3"
             , "--no-sandbox"
             , "--mute-audio=1"
             , "--disable-features=WebRtcHideLocalIpsWithMdns"
             , "--disable-setuid-sandbox"
             , "--use-fake-ui-for-media-stream=1"
             , "--use-fake-device-for-media-stream=1"
             , "--allow-running-insecure-content"
             , "--ignore-certificate-errors"
             , "--unsafely-treat-insecure-origin-as-secure"
             ]

appUrl :: T.URL
appUrl = T.URL "http://localhost:3080"

main :: Effect Unit
main =
  launchAff_
    $ un Identity $ runSpecT testConfig [ consoleReporter ] do
      describe "WebRTC browser" do
        it "can check that a streaming video has started and is playing" do
          browser <- T.launch { headless: false
                              , args: launchArgs
                              , devtools: true
                              }
          page <- T.newPage browser
          -- inject JS into the page
          jsFile <- readTextFile UTF8 "./scripts/injectWebRTC.js"
          _ <- T.unsafeEvaluateOnNewDocument jsFile page
          T.goto appUrl page
          _ <- delay (Milliseconds 1000.00)
          stats1 <- getVideoStats page
          -- wait for the dom and video then get video stats
          _ <- delay (Milliseconds 5000.00)
          stats2 <- getVideoStats page

          Assert.assert "frames haven't been decoded" (stats2.framesDecoded > stats1.framesDecoded)
          Assert.assert "packets haven't been decoded" (stats2.packetsReceived > stats1.packetsReceived)
          T.close browser
