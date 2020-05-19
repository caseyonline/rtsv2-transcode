module Cases.WebRTCTest where

import Prelude

import Data.Either
import Data.Identity (Identity(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Newtype (un, wrap, unwrap)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Foreign (readString, unsafeFromForeign)
import Helpers.Assert as A
import Helpers.CreateString as C
import Helpers.Env as E
import Helpers.Env as Env
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log as L
import Helpers.OsCmd (runProc)
import Helpers.RTCPeerConnection (getVideoStats)
import Helpers.Types (Node)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Shared.Rtsv2.Router.Endpoint (Endpoint(..), makeUrl, makeUrlAddr)
import Shared.Rtsv2.Stream (SlotRole(..), RtmpShortName, RtmpStreamName, SlotId)
import Shared.Utils (lazyCrashIfMissing)
import Shared.UUID as UUID
import Test.Spec (SpecT, describe, describeOnly, it, itOnly, before_, after_)
import Test.Spec.Runner (Config)
import Test.Unit as Test
import Test.Unit.Assert as Assert
import Toppokki as T

-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------
launchArgs :: Array String
launchArgs =
  [ "--allow-running-insecure-content"
  , "--disable-features=WebRtcHideLocalIpsWithMdns"
  , "--disable-infobars"
  , "--disable-setuid-sandbox"
  , "--disable-sync"
  , "--disable-web-security"
  , "--enable-media-stream"
  , "--enable-translate-new-ux"
  , "--ignore-certificate-errors"
  , "--mute-audio=1"
  , "--no-default-browser-check"
  , "--no-sandbox"
  , "--reduce-security-for-testing"
  , "--use-fake-device-for-media-stream"
  , "--use-fake-ui-for-media-stream"
  , "--vmodule=*/webrtc/*=3"
  ]

-- This is used to for the webRtcIngest tests
launchArgs2 :: Array String
launchArgs2 =
  [
    "--disable-infobars"
  , "--disable-sync"
  , "--disable-web-security"
  , "--enable-translate-new-ux"
  , "--no-default-browser-check"
  , "--no-sandbox"
  , "--reduce-security-for-testing"
  , "--unsafely-allow-protected-media-identifier-for-domain"
  , "--unsafely-treat-insecure-origin-as-secure=http://172.16.169.1:3000"
  , "--use-fake-ui-for-media-stream"
  ]

options =
  { headless: false
  , args: launchArgs2
  , devtools: true
  }

-------------------------------------------------------------------------------
-- Runner
-------------------------------------------------------------------------------
webRTCTest :: forall m. Monad m => SpecT Aff Unit m Unit
webRTCTest =
  describe "WebRTC browser tests" do
    primaryStream
    backupStream
    ingestStream
    webRtcIngest


-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------
primaryStream :: forall m. Monad m => SpecT Aff Unit m Unit
primaryStream =
  describe "5.1 Primary Stream tests" do
    before_ (F.startSession [Env.p1n1] *> F.launch [Env.p1n1] *> F.startSlotHigh1000 (C.toAddrFromNode Env.p1n1)) do
      after_ (F.stopSession *> F.stopSlot) do
        it "5.1.1 can check that a streaming video has started and is playing on Primary" do
          _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for ingest to start fully"
          browser <- T.launch options
          page <- T.newPage browser

          -- inject JS into the page
          -- jsFile <- readTextFile UTF8 "./scripts/injectWebRTC.js"
          -- _ <- T.unsafeEvaluateOnNewDocument jsFile page

          T.goto (HTTP.playerUrl Env.p1n1 E.slot1 Primary) page
          _ <- delay (Milliseconds 4000.00) >>= L.as' "wait for video to start"

          frames1 <- getInnerText "#frames" page
          packets1 <- getInnerText "#packets" page

          _ <- delay (Milliseconds 3000.00) >>= L.as' "let video play for 3 seconds"

          frames2 <- getInnerText "#frames" page
          packets2 <- getInnerText "#packets" page

          let frameDiff = stringToInt frames2 - stringToInt frames1

          Assert.assert "frames aren't increasing" (frameDiff > 65) >>= L.as' ("frames increased by: " <> show frameDiff)
          Assert.assert "packets aren't increasing" ((stringToInt packets1) < (stringToInt packets2)) >>= L.as' ("packets are increasing: " <> packets2 <> " > " <> packets1)
          T.close browser


backupStream :: forall m. Monad m => SpecT Aff Unit m Unit
backupStream =
  describe "5.2 Backup Stream tests" do
    before_ (F.startSession [Env.p1n1] *> F.launch [Env.p1n1] *> F.startSlotHigh1000Backup (C.toAddrFromNode Env.p1n1)) do
      after_ (F.stopSession *> F.stopSlot) do
        it "5.2.1 can check that a streaming video has started and is playing on Backup" do
          _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for ingest to start fully"
          browser <- T.launch options
          page <- T.newPage browser

          T.goto (HTTP.playerUrl Env.p1n1 E.slot1 Backup) page
          _ <- delay (Milliseconds 3000.00) >>= L.as' "wait for video to start"

          frames1 <- getInnerText "#frames" page
          packets1 <- getInnerText "#packets" page

          _ <- delay (Milliseconds 3000.00) >>= L.as' "let video play for 3 seconds"

          frames2 <- getInnerText "#frames" page
          packets2 <- getInnerText "#packets" page

          let frameDiff = stringToInt frames2 - stringToInt frames1

          Assert.assert ("frames aren't increasing: " <> show frameDiff) (frameDiff > 65) >>= L.as' ("frames increased by: " <> show frameDiff)
          Assert.assert "packets aren't increasing" ((stringToInt packets1) < (stringToInt packets2)) >>= L.as' ("packets are increasing: " <> packets2 <> " > " <> packets1)
          T.close browser

ingestNodes :: Array Node
ingestNodes = [E.p1n1, E.p1n2, E.p1n3]

ingestStream :: forall m. Monad m => SpecT Aff Unit m Unit
ingestStream =
  describe "5.3 Ingest Stream tests" do
    before_ (F.startSession ingestNodes *> F.launch ingestNodes) do
      after_ (F.stopSession *> F.stopSlot) do
        it "5.3.1 webrtc ingest on different node to aggregator" do
          traverse_ F.maxOut (F.allNodesBar E.p1n1 ingestNodes) >>= L.as' "load up all servers bar one"
          E.waitForIntraPoPDisseminate
          F.startSlotHigh1000 (C.toAddrFromNode Env.p1n2) >>= L.as' "create high ingest"
          _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for ingest to start fully"

          browser <- T.launch options
          page <- T.newPage browser

          T.goto (HTTP.playerUrl Env.p1n1 E.slot1 Primary) page
          _ <- delay (Milliseconds 3000.00) >>= L.as' "wait for video to start"

          frames1 <- getInnerText "#frames" page
          packets1 <- getInnerText "#packets" page

          _ <- delay (Milliseconds 3000.00) >>= L.as' "let video play for 3 seconds"

          frames2 <- getInnerText "#frames" page
          packets2 <- getInnerText "#packets" page

          let frameDiff = stringToInt frames2 - stringToInt frames1

          Assert.assert "frames aren't increasing" (frameDiff > 65) >>= L.as' ("frames increased by: " <> show frameDiff)
          Assert.assert "packets aren't increasing" ((stringToInt packets1) < (stringToInt packets2)) >>= L.as' ("packets are increasing: " <> packets2 <> " > " <> packets1)
          T.close browser


webRtcIngest :: forall m. Monad m => SpecT Aff Unit m Unit
webRtcIngest =
  describe "5.4 webRtc Ingest tests" do
    before_ (F.startSession ingestNodes *> F.launch ingestNodes) do
      after_ (F.stopSession *> F.stopSlot) do
        it "5.4.1 can check that a streaming video has started and is playing on Backup" do
          traverse_ F.maxOut (F.allNodesBar E.p1n1 ingestNodes) >>= L.as' "load up all servers bar one"
          E.waitForIntraPoPDisseminate

          browser <- T.launch options
          page <- T.newPage browser
          T.goto (HTTP.ingestUrl Env.p1n1 E.shortName1 E.highStreamName) page
          _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for page to load"

          T.click (T.Selector "#authenticate") page
          _ <- delay (Milliseconds 500.00) >>= L.as' "wait for authentication"

          T.click (T.Selector "#start-ingest") page
          _ <- delay (Milliseconds 2000.00) >>= L.as' "let stream start"

          byteSent <- getInnerText "#bytesSent" page

          _ <- delay (Milliseconds 2000.00) >>= L.as' "let stream start"
          Assert.assert "Bytes are being sent in the UI" ((stringToInt byteSent) > 10) >>= L.as' ("frames increased by: " <> byteSent)

          HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 200
                                                 >>= A.assertAggregator [E.highSlotAndProfileName]
                                                 >>= L.as "aggregator created on idle server"

          (traverse_ (F.aggregatorNotPresent E.slot1)
            $ F.allNodesBar E.p1n1 ingestNodes)  >>= L.as' "aggregator not on busy servers"

          T.click (T.Selector "#stop-ingest") page
          E.waitForAsyncProfileStop
          HTTP.getAggregatorStats E.p1n1 E.slot1 >>= A.assertStatusCode 200
                                                 >>= A.assertAggregator []  >>= L.as "aggregator has no profiles"

          T.close browser

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------
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
