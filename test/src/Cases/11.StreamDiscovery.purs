module Cases.StreamDiscovery (streamDiscoveryTests) where

import Prelude

import Effect.Aff (Aff, Milliseconds(..), delay)
import Helpers.Assert (assertBodyTextFun, assertStatusCode)
import Helpers.Env as E
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log as L
import Helpers.Types (Node)
import Test.Spec (SpecT, after, before, describe, describeOnly, it)
import Toppokki as T

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
streamDiscoveryTests :: forall m. Monad m => SpecT Aff Unit m Unit
streamDiscoveryTests = do
  describe "11 Stream discovery tests" do
    streamDiscovery

-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------
nodes :: Array Node
nodes = [E.p1n1, E.p1n2, E.p1n3]

options :: { args :: Array String
           , devtools :: Boolean
           , headless :: Boolean
           }
options =
  { headless: false
  , args: E.browserLaunchArgsIng
  , devtools: true
  }

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------
streamDiscovery :: forall m. Monad m => SpecT Aff Unit m Unit
streamDiscovery =
  describe "10.1 Ingest metrics" do
    before (F.startSession [E.p1n1] *> F.launch [E.p1n1] *> T.launch options) do
      after (\browser -> T.close browser *> F.stopSession *> F.stopSlot) do
        it "10.1.1 ingest bytes should increase " $ \browser -> do

          HTTP.getStreamDiscovery E.p1n1 E.shortName1 E.highStreamName
               >>= assertStatusCode 404
               >>= L.as "No ingest started so returns 404"

          page <- T.newPage browser
          T.goto (HTTP.ingestUrl E.p1n1 E.shortName1 E.highStreamName) page
          delay (Milliseconds 2000.00) >>= L.as' "wait for page to load"

          T.click (T.Selector "#authenticate") page
          delay (Milliseconds 500.00) >>= L.as' "wait for authentication"

          T.click (T.Selector "#start-ingest") page
          delay (Milliseconds 6000.00) >>= L.as' "let stream start"

          HTTP.getStreamDiscovery E.p1n1 E.shortName1 E.highStreamName
            >>= assertStatusCode 200
            >>= assertBodyTextFun (\a -> a /= "")
            >>= L.as "Ingest discovery returns non empty URL"
