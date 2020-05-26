module Cases.BrowserIngestTest where

import Prelude

import Data.Traversable (traverse_)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Helpers.Assert as A
import Helpers.CreateString as C
import Helpers.Env as E
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log as L
import Helpers.Types (Node)
<<<<<<< HEAD:test/src/Cases/BrowserIngestTest.purs
import Shared.Rtsv2.Stream (SlotRole(..), RtmpShortName, RtmpStreamName, SlotId)
import Test.Spec (SpecT, describe, it, before_, after_)
=======
import Shared.Rtsv2.Stream (SlotRole(..))
import Test.Spec (SpecT, describe, it, itOnly, before_, after_)
>>>>>>> master:test/src/Cases/5.BrowserIngestTest.purs
import Test.Unit.Assert as Assert
import Toppokki as T


-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------

options =
  { headless: false
  , args: E.browserLaunchArgsIng
  , devtools: true
  }

-------------------------------------------------------------------------------
-- Runner
-------------------------------------------------------------------------------
browserIngestTest :: forall m. Monad m => SpecT Aff Unit m Unit
browserIngestTest =
  describe "WebRTC browser tests" do
    primaryStream -- 5.1
    backupStream -- 5.2
    ingestStream -- 5.3
    webRtcIngest -- 5.4

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------
primaryStream :: forall m. Monad m => SpecT Aff Unit m Unit
primaryStream =
  describe "5.1 Primary Stream tests" do
    before_ (F.startSession [E.p1n1] *> F.launch [E.p1n1] *> F.startSlotHigh1000 (C.toAddrFromNode E.p1n1)) do
      after_ (F.stopSession *> F.stopSlot) do
        it "5.1.1 can check that a streaming video has started and is playing on Primary" do
          _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for ingest to start fully"
          browser <- T.launch options
          page <- T.newPage browser

          -- inject JS into the page
          -- jsFile <- readTextFile UTF8 "./scripts/injectWebRTC.js"
          -- _ <- T.unsafeEvaluateOnNewDocument jsFile page

          T.goto (HTTP.playerUrl E.p1n1 E.slot1 Primary) page
          _ <- delay (Milliseconds 4000.00) >>= L.as' "wait for video to start"

          frames1 <- F.getInnerText "#frames" page
          packets1 <- F.getInnerText "#packets" page

          _ <- delay (Milliseconds 3000.00) >>= L.as' "let video play for 3 seconds"

          frames2 <- F.getInnerText "#frames" page
          packets2 <- F.getInnerText "#packets" page

          let frameDiff = F.stringToInt frames2 - F.stringToInt frames1

          Assert.assert "frames aren't increasing" (frameDiff > 65) >>= L.as' ("frames increased by: " <> show frameDiff)
          Assert.assert "packets aren't increasing" ((F.stringToInt packets1) < (F.stringToInt packets2)) >>= L.as' ("packets are increasing: " <> packets2 <> " > " <> packets1)
          T.close browser

backupStream :: forall m. Monad m => SpecT Aff Unit m Unit
backupStream =
  describe "5.2 Backup Stream tests" do
    before_ (F.startSession [E.p1n1] *> F.launch [E.p1n1] *> F.startSlotHigh1000Backup (C.toAddrFromNode E.p1n1)) do
      after_ (F.stopSession *> F.stopSlot) do
        it "5.2.1 can check that a streaming video has started and is playing on Backup" do
          _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for ingest to start fully"
          browser <- T.launch options
          page <- T.newPage browser

          T.goto (HTTP.playerUrl E.p1n1 E.slot1 Backup) page
          _ <- delay (Milliseconds 3000.00) >>= L.as' "wait for video to start"

          frames1 <- F.getInnerText "#frames" page
          packets1 <- F.getInnerText "#packets" page

          _ <- delay (Milliseconds 3000.00) >>= L.as' "let video play for 3 seconds"

          frames2 <- F.getInnerText "#frames" page
          packets2 <- F.getInnerText "#packets" page

          let frameDiff = F.stringToInt frames2 - F.stringToInt frames1

          Assert.assert ("frames aren't increasing: " <> show frameDiff) (frameDiff > 65) >>= L.as' ("frames increased by: " <> show frameDiff)
          Assert.assert "packets aren't increasing" ((F.stringToInt packets1) < (F.stringToInt packets2)) >>= L.as' ("packets are increasing: " <> packets2 <> " > " <> packets1)
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
          F.startSlotHigh1000 (C.toAddrFromNode E.p1n2) >>= L.as' "create high ingest"
          _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for ingest to start fully"

          browser <- T.launch options
          page <- T.newPage browser

          T.goto (HTTP.playerUrl E.p1n1 E.slot1 Primary) page
          _ <- delay (Milliseconds 3000.00) >>= L.as' "wait for video to start"

          frames1 <- F.getInnerText "#frames" page
          packets1 <- F.getInnerText "#packets" page

          _ <- delay (Milliseconds 3000.00) >>= L.as' "let video play for 3 seconds"

          frames2 <- F.getInnerText "#frames" page
          packets2 <- F.getInnerText "#packets" page

          let frameDiff = F.stringToInt frames2 - F.stringToInt frames1

          Assert.assert "frames aren't increasing" (frameDiff > 65) >>= L.as' ("frames increased by: " <> show frameDiff)
          Assert.assert "packets aren't increasing" ((F.stringToInt packets1) < (F.stringToInt packets2)) >>= L.as' ("packets are increasing: " <> packets2 <> " > " <> packets1)
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
          T.goto (HTTP.ingestUrl E.p1n1 E.shortName1 E.highStreamName) page
          _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for page to load"

          T.click (T.Selector "#authenticate") page
          _ <- delay (Milliseconds 500.00) >>= L.as' "wait for authentication"

          T.click (T.Selector "#start-ingest") page
          _ <- delay (Milliseconds 2000.00) >>= L.as' "let stream start"

          byteSent <- F.getInnerText "#bytesSent" page

          _ <- delay (Milliseconds 2000.00) >>= L.as' "let stream start"
          Assert.assert "Bytes are being sent in the UI" ((F.stringToInt byteSent) > 10) >>= L.as' ("frames increased by: " <> byteSent)

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
