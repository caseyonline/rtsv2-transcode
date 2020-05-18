module Cases.BrowserDataMsging where

import Prelude

import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Helpers.Assert as A
import Helpers.CreateString as C
import Helpers.Env as E
import Helpers.Env as Env
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log as L
import Helpers.Types (Node)
import Shared.Rtsv2.Router.Endpoint (Endpoint(..), makeUrl)
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

          browser1 <- T.launch options
          page1 <- T.newPage browser1
          page2 <- T.newPage browser1

          T.goto (F.mkPlayerUrl Env.p1n1 E.slot1 Primary) page1
          T.goto (F.mkPlayerUrl Env.p1n2 E.slot1 Primary) page2
          _ <- delay (Milliseconds 3000.00) >>= L.as' "wait for video to start"

          frames1 <- F.getInnerText "#frames" page1
          packets1 <- F.getInnerText "#packets" page1

          _ <- delay (Milliseconds 3000.00) >>= L.as' "let video play for 3 seconds"

          frames2 <- F.getInnerText "#frames" page1
          packets2 <- F.getInnerText "#packets" page1

          let frameDiff = F.stringToInt frames2 - F.stringToInt frames1

          Assert.assert "frames aren't increasing" (frameDiff > 70) >>= L.as' ("frames increased by: " <> show frameDiff)
          Assert.assert "packets aren't increasing" ((F.stringToInt packets1) < (F.stringToInt packets2)) >>= L.as' ("packets are increasing: " <> packets2 <> " > " <> packets1)
          T.close browser1
