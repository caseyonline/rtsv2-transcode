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
import Test.Spec (SpecT, describe, it, before_, after_)
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
  describe "WebRTC browser tests" do
    ingestStream -- 5.3

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

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

          browser1 <- T.launch options
          page1 <- T.newPage browser1
          page2 <- T.newPage browser1

          T.goto (F.mkPlayerUrl E.p1n1 E.slot1 Primary) page1
          T.goto (F.mkPlayerUrl E.p1n2 E.slot1 Primary) page2
          _ <- delay (Milliseconds 3000.00) >>= L.as' "wait for webRTC connection"

          -- traceIds aren't showing on UI in tests
          -- traceId1 <- F.getInnerText "#traceId" page1
          -- traceId2 <- F.getInnerText "#traceId" page2

          T.focus (T.Selector "input#msginput") page2
          T.keyboardSendCharacter "Hello World" page2
          T.click (T.Selector "button#msgsend") page2

          _ <- delay (Milliseconds 250.00) >>= L.as' "wait message to send"
          -- need to change page
          T.bringToFront page1
          T.focus (T.Selector "input#msginput") page1
          T.keyboardSendCharacter "Oh hi World" page1
          T.click (T.Selector "button#msgsend") page1

          msgSentTraceId <- F.getInnerText "div.message.msg_sent > div.msg_name" page2
          msgReceivedTraceId <- F.getInnerText "div.message.msg_received > div.msg_name" page1


          Assert.assert "Same sent and received TraceIds" (msgSentTraceId == msgReceivedTraceId) >>= L.as' ("Sent: " <> show msgSentTraceId <> " Received: " <> msgReceivedTraceId )
          T.close browser1
