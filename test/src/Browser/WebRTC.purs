module WebRTC where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Foreign (unsafeFromForeign)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (Config, runSpecT)
import Test.Unit.Assert as Assert
import Toppokki as T

testConfig :: Config
testConfig = { slow: (Milliseconds 500.00), timeout: Just (Milliseconds 100000.00), exit: false }

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
        it "can do stuff" do
          browser <- T.launch { headless: false
                              , devtools: true
                              , args: launchArgs
                              }
          page <- T.newPage browser
          jsFile <- readTextFile UTF8 "./scripts/inject.js"
          _ <- T.unsafeEvaluateOnNewDocument jsFile page
          T.goto appUrl page
          _ <- delay (Milliseconds 500000.00)
          innerTextF <- T.unsafePageEval
            (T.Selector "#eval-inject")
            "el => el.innerText"
            page
          Assert.equal "345" ((unsafeFromForeign innerTextF) :: String)
          T.close browser
