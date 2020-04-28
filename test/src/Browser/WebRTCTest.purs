module Browser.WebRTCTest where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe)
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
import Test.Spec (SpecT, describe, describeOnly, it, itOnly, before_, after_)
import Test.Spec.Runner (Config)
import Test.Unit.Assert as Assert
import Test.Unit as Test
import Toppokki as T
import Foreign (readString, unsafeFromForeign)
import Data.Int (fromString)


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

appUrl :: Node -> String -> T.URL
appUrl node pb = T.URL $ "http:"
              <> C.toAddrFromNode node
              <> ":3000/public/canary/client/00000000-0000-0000-0000-000000000001/"
              <> pb
              <> "/player"

webRTCTest :: forall m. Monad m => SpecT Aff Unit m Unit
webRTCTest =
  describe "WebRTC browser tests" do
    primaryStream
    backupStream


primaryStream :: forall m. Monad m => SpecT Aff Unit m Unit
primaryStream =
  describe "Primary Stream tests" do
    before_ (F.startSession [Env.p1n1] *> F.launch [Env.p1n1] *> F.startSlotHigh1000 (C.toAddrFromNode Env.p1n1)) do
      after_ (F.stopSession *> F.stopSlot) do
        it "can check that a streaming video has started and is playing on Primary" do
          _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for ingest to start fully"
          browser <- T.launch { headless: false
                              , args: launchArgs
                              , devtools: true
                              }
          page <- T.newPage browser

          -- inject JS into the page
          -- jsFile <- readTextFile UTF8 "./scripts/injectWebRTC.js"
          -- _ <- T.unsafeEvaluateOnNewDocument jsFile page

          T.goto (appUrl Env.p1n1 "primary") page
          _ <- delay (Milliseconds 4000.00) >>= L.as' "wait for video to start"

          frames1 <- getInnerText "#frames" page
          packets1 <- getInnerText "#packets" page

          _ <- delay (Milliseconds 3000.00) >>= L.as' "let video play for 3 seconds"

          frames2 <- getInnerText "#frames" page
          packets2 <- getInnerText "#packets" page

          let frameDiff = stringToInt frames2 - stringToInt frames1

          _ <- L.as' ("frames increased by: " <> show frameDiff) ""

          Assert.assert "frames aren't increasing" (frameDiff > 70) >>= L.as' ("frames increased by: " <> show frameDiff)
          Assert.assert "packets aren't increasing" ((stringToInt packets1) < (stringToInt packets2)) >>= L.as' ("packets are increasing: " <> packets2 <> " > " <> packets1)
          T.close browser



backupStream :: forall m. Monad m => SpecT Aff Unit m Unit
backupStream =
  describe "Backup Stream tests" do
    before_ (F.startSession [Env.p1n1] *> F.launch [Env.p1n1] *> F.startSlotHigh1000Backup (C.toAddrFromNode Env.p1n1)) do
      after_ (F.stopSession *> F.stopSlot) do
        it "can check that a streaming video has started and is playing on Backup" do
          _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for ingest to start fully"
          browser <- T.launch { headless: false
                              , args: launchArgs
                              , devtools: true
                              }
          page <- T.newPage browser

          T.goto (appUrl Env.p1n1 "backup") page
          _ <- delay (Milliseconds 3000.00) >>= L.as' "wait for video to start"

          frames1 <- getInnerText "#frames" page
          packets1 <- getInnerText "#packets" page

          _ <- delay (Milliseconds 3000.00) >>= L.as' "let video play for 3 seconds"

          frames2 <- getInnerText "#frames" page
          packets2 <- getInnerText "#packets" page

          let frameDiff = stringToInt frames2 - stringToInt frames1

          Assert.assert "frames aren't increasing" (frameDiff > 70) >>= L.as' ("frames increased by: " <> show frameDiff)
          Assert.assert "packets aren't increasing" ((stringToInt packets1) < (stringToInt packets2)) >>= L.as' ("packets are increasing: " <> packets2 <> " > " <> packets1)
          T.close browser




getInnerText :: String -> T.Page -> Aff String
getInnerText selector page = do
  innerTextF <- T.unsafePageEval
                (T.Selector selector)
                "el => el.innerText"
                page
  let innerText = (unsafeFromForeign innerTextF) :: String
  pure innerText


stringToInt :: String -> Int
stringToInt s =
  fromMaybe 0 $ fromString s
