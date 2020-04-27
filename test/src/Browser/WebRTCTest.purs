module Browser.WebRTCTest where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Either
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Helpers.Assert as A
import Helpers.CreateString as C
import Helpers.Env as E
import Helpers.Env as Env
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log as L
import Helpers.OsCmd (runProc)
import Helpers.Types (Node)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import RTCPeerConnection (getVideoStats)
import Test.Spec (SpecT, describe, it, before_, after_)
import Test.Spec.Runner (Config)
import Test.Unit.Assert as Assert
import Test.Unit as Test
import Toppokki as T
import Foreign (readString, unsafeFromForeign)
import Data.Int (fromString)

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

appUrl :: Node -> T.URL
appUrl node = T.URL $ "http:" <> C.toAddrFromNode node <> ":3000/public/canary/client/00000000-0000-0000-0000-000000000001/primary/player"

webRTCTest :: forall m. Monad m => SpecT Aff Unit m Unit
webRTCTest =
  describe "WebRTC browser tests" do
    before_ (F.startSession [Env.p1n1] *> F.launch [Env.p1n1] *> (F.startSlotHigh1000 (C.toAddrFromNode Env.p1n1)) ) do
      after_ F.stopSession do
        it "can check that a streaming video has started and is playing" do

          browser <- T.launch { headless: false
                              , args: launchArgs
                              , devtools: true
                              }
          page <- T.newPage browser

          -- inject JS into the page
          -- jsFile <- readTextFile UTF8 "./scripts/injectWebRTC.js"
          -- _ <- T.unsafeEvaluateOnNewDocument jsFile page

          T.goto (appUrl Env.p1n1) page
          _ <- delay (Milliseconds 2000.00) >>= L.as' "waited video to start"

          frames1 <- getInnerText "#frames" page
          packets1 <- getInnerText "#packets" page

          _ <- delay (Milliseconds 3000.00) >>= L.as' "let video play for 3 seconds"

          frames2 <- getInnerText "#frames" page
          packets2 <- getInnerText "#packets" page

          Assert.assert "frames aren't increasing" ( frames1 < frames2) >>= L.as' ("frames are increasing: " <> frames1 <> " > " <> frames2)
          Assert.assert "packets aren't increasing" ( packets1 < packets2) >>= L.as' ("packets are increasing: " <> packets1 <> " > " <> packets2)
          T.close browser


getInnerText :: String -> T.Page -> Aff String
getInnerText selector page = do
  innerTextF <- T.unsafePageEval
                (T.Selector selector)
                "el => el.innerText"
                page
  let innerText = (unsafeFromForeign innerTextF) :: String
  pure innerText

-- stringToInt :: String -> Either (Aff Unit) Int
-- stringToInt s =
--   case fromString s of
--     Left -> Test.failure "DOM value isn't an Int"
--     Just int -> Right int
