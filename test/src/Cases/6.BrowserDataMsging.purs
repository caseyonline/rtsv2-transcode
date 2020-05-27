module Cases.BrowserDataMsging where

import Prelude

import Data.Traversable (traverse_)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Helpers.Assert as A
import Helpers.CreateString as C
import Helpers.Env as E
import Helpers.Functions as F
import Helpers.Log as L
import Helpers.Types (Node)
import Shared.Rtsv2.Stream (SlotRole(..), RtmpShortName, RtmpStreamName, SlotId)
import Test.Spec (SpecT, describe, describeOnly, it, before_, after_)
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
browserDataMsging :: forall m. Monad m => SpecT Aff Unit m Unit
browserDataMsging =
  describeOnly "WebRTC browser tests" do
    ingestStream -- 5.3

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

ingestNodes :: Array Node
ingestNodes = [E.p1n1, E.p1n2, E.p2n1]

ingestStream :: forall m. Monad m => SpecT Aff Unit m Unit
ingestStream =
  describe "5.3 Ingest Stream tests" do
    before_ (F.startSession ingestNodes *> F.launch ingestNodes) do
      after_ (F.stopSession *> F.stopSlot) do
        it "5.3.1 webrtc ingest on different node to aggregator" do

          F.startSlotHigh1000 (C.toAddrFromNode E.p1n1) >>= L.as' "create high ingest on 1"
          F.startSlotHigh1000Backup (C.toAddrFromNode E.p1n1) >>= L.as' "create high backup on 1"
          F.start2SlotHigh1000 (C.toAddrFromNode E.p2n1) >>= L.as' "create high ingest on 2"

          _ <- delay (Milliseconds 5000.00) >>= L.as' "wait for ingest to start fully"

          browser1 <- T.launch options

          tab1 <- T.newPage browser1
          tab2 <- T.newPage browser1
          tab3 <- T.newPage browser1
          tab4 <- T.newPage browser1

          T.goto (F.mkPlayerUrl E.p1n1 E.slot1 Primary) tab1
          T.goto (F.mkPlayerUrl E.p1n2 E.slot1 Primary) tab2
          T.goto (F.mkPlayerUrl E.p2n1 E.slot2 Primary) tab3
          T.goto (HTTP.ingestUrl E.p1n1 E.shortName1 E.highStreamName) tab4

          _ <- delay (Milliseconds 2000.00) >>= L.as' "wait for tabs to load"
          traceId1 <- F.getInnerText "#traceId" tab1
          traceId2 <- F.getInnerText "#traceId" tab2
          traceId3 <- F.getInnerText "#traceId" tab3
          -- traceId4 <- F.getInnerText "#traceId" tab4

          -- go to tab2 send a message
          T.bringToFront tab2
          T.focus (T.Selector "input#msginput") tab2
          T.keyboardSendCharacter "Hello World" tab2
          T.click (T.Selector "button#msgsend") tab2

          _ <- delay (Milliseconds 250.00) >>= L.as' "wait message to send"
          msgSentTraceId <- F.getInnerText "div.message.msg_sent > div.msg_name" tab2

          -- Go to tab 1 check message was sent
          T.bringToFront tab1
          T.focus (T.Selector "input#msginput") tab1
          T.keyboardSendCharacter "Oh hi World" tab1
          T.click (T.Selector "button#msgsend") tab1

          _ <- delay (Milliseconds 60000.00) >>= L.as' "wait for webRTC connection"
          msgReceivedTraceId <- F.getInnerText "div.message.msg_received > div.msg_name" tab1


          Assert.assert "Same sent and received TraceIds" (msgSentTraceId == msgReceivedTraceId)
            >>= L.as' ("Sent: " <> show msgSentTraceId <> " Received: " <> msgReceivedTraceId )
          T.close browser1
