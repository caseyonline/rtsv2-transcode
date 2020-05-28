module Cases.BrowserDataMsging where

import Prelude

import Effect.Aff (Aff, Milliseconds(..), delay)
import Helpers.CreateString as C
import Helpers.Env as E
import Helpers.HTTP as HTTP
import Helpers.Functions as F
import Helpers.Log as L
import Helpers.Types (Node)
import Shared.Rtsv2.Stream (SlotRole(..))
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

firstMessage :: String
firstMessage = "Hello everyone"

secondMessage :: String
secondMessage = "Oh hi"

-------------------------------------------------------------------------------
-- Runner
-------------------------------------------------------------------------------
browserDataMsging :: forall m. Monad m => SpecT Aff Unit m Unit
browserDataMsging =
  describe "Data Messages" do
    broadcastMessages -- 5.3

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

ingestNodes :: Array Node
ingestNodes = [E.p1n1, E.p1n2, E.p2n1]

broadcastMessages :: forall m. Monad m => SpecT Aff Unit m Unit
broadcastMessages =
  describe "6.1 Broadcast Messages " do
    before_ (F.startSession ingestNodes *> F.launch ingestNodes) do
      after_ (F.stopSession *> F.stopSlot) do
        it "6.1.1 broadcast messages accross slots" do

          F.startSlotHigh1000 (C.toAddrFromNode E.p1n1) >>= L.as' "create high ingest on 1"
          F.startSlotHigh1000Backup (C.toAddrFromNode E.p1n1) >>= L.as' "create high backup on 1"
          F.start2SlotHigh1000 (C.toAddrFromNode E.p2n1) >>= L.as' "create high ingest on 2"

          _ <- delay (Milliseconds 5000.00) >>= L.as' "wait for ingest to start fully"

          browser1 <- T.launch options

          -- create the different tabs
          tab1 <- T.newPage browser1
          tab2 <- T.newPage browser1
          tab3 <- T.newPage browser1
          tab4 <- T.newPage browser1

          -- navigate to specific Urls on each tab
          T.goto (F.mkPlayerUrl E.p1n1 E.slot1 Primary) tab1
          T.goto (F.mkPlayerUrl E.p1n2 E.slot1 Primary) tab2
          T.goto (F.mkPlayerUrl E.p2n1 E.slot2 Primary) tab3
          T.goto (HTTP.ingestUrl E.p1n1 E.shortName1 E.highStreamName) tab4

          _ <- delay (Milliseconds 1000.00) >>= L.as' "wait for tabs to load"

          -- get the traceIds from each tab
          traceId1 <- F.getInnerText "#traceId" tab1
          traceId2 <- F.getInnerText "#traceId" tab2
          traceId3 <- F.getInnerText "#traceId" tab3
          -- traceId4 <- F.getInnerText "#traceId" tab4

          -- go to tab2 send a message
          T.bringToFront tab2 >>= L.as' "Send firstMessage from tab2"
          T.focus (T.Selector "input#msginput") tab2
          T.keyboardSendCharacter firstMessage tab2
          T.click (T.Selector "button#msgsend") tab2

          -- Go to tab 1 check message was sent
          T.bringToFront tab1 >>= L.as' "Check firstMessage received in tab1"
          msgReceivedTraceId1 <- F.getInnerText "div.message.msg_received > div.msg_name" tab1
          msgContent1 <- F.getInnerText "div.message.msg_received > div.msg_bubble" tab1


          Assert.assert "TraceId matches on first message sent and received" (traceId2 == msgReceivedTraceId1)
            >>= L.as' ("Sent: " <> show traceId2 <> " Received: " <> msgReceivedTraceId1)
          Assert.assert "Message content matched first sent message" (firstMessage == msgContent1)
            >>= L.as' ("Sent: " <> firstMessage <> " Received: " <> msgContent1)

          -- Send a message back
          T.focus (T.Selector "input#msginput") tab1 >>= L.as' "Send secondMessage from tab2"
          T.keyboardSendCharacter secondMessage tab1
          T.click (T.Selector "button#msgsend") tab1

          -- Got to tab2 check that new message has been recieved
          T.bringToFront tab2 >>= L.as' "Check secondMessage received in tab1"
          msgReceivedTraceId2 <- F.getInnerText "div.message.msg_received > div.msg_name" tab2
          msgContent2 <- F.getInnerText "div.message.msg_received > div.msg_bubble" tab2

          Assert.assert "TraceId matches on second message sent and received" (traceId1 == msgReceivedTraceId2)
            >>= L.as' ("Sent: " <> show traceId1 <> " Received: " <> msgReceivedTraceId2)
          Assert.assert "Message content matched second message sent" (secondMessage == msgContent2)
            >>= L.as' ("Sent: " <> secondMessage <> " Received: " <> msgContent2)

          -- Got to tab3 that there are no messages as it's a different slot
          T.bringToFront tab3 >>= L.as' "Check tab3 has no messages as on different slot"
          msgContent3 <- F.getInnerText "div.messages" tab3

          Assert.assert "Message content should be empty as this is on different slot" ("" == msgContent3)
            >>= L.as' ("Tab3 message content: " <> msgContent3)

          T.close browser1