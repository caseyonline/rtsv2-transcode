module Cases.Metrics (metricsTests) where

import Prelude

import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import Effect.Aff (Aff, Error, Milliseconds(..), delay, throwError)
import Effect.Exception as Ex
import Helpers.Assert (assertStatusCode)
import Helpers.CreateString as C
import Helpers.Env as E
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log as L
import Helpers.Types (Node, ResWithBody)
import Shared.Rtsv2.Stream (SlotRole(..))
import Test.Spec (SpecT, after_, after, before, before_, describe, it)
import Test.Unit.Assert as Assert
import Toppokki as T

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
metricsTests :: forall m. Monad m => SpecT Aff Unit m Unit
metricsTests = do
  describe "10 metrics tests" do
    ingestMetrics
    egestMetrics

-------------------------------------------------------------------------------
-- Vars
-------------------------------------------------------------------------------
nodes :: Array Node
nodes = [E.p1n1, E.p1n2, E.p1n3]

options =
  { headless: false
  , args: E.browserLaunchArgsIng
  , devtools: true
  }

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------
ingestMetrics :: forall m. Monad m => SpecT Aff Unit m Unit
ingestMetrics =
  describe "10.1 Ingest metrics" do
    before_ (F.startSession [E.p1n1] *> F.launch [E.p1n1] *> F.startSlotHigh1000 (C.toAddrFromNode E.p1n1)) do
      after_ (F.stopSession *> F.stopSlot) do
        it "10.1.1 ingest bytes should increase " do
          E.waitForAsyncProfileStart >>= L.as' "wait for async start of profile"

          response1 <- assertStatusCode 200 =<< HTTP.getIngestMetrics E.p1n1
          body1 <- getResBody response1
          let bytes1 = fromMaybe 0 $ getValue $ getLine 5 body1

          delay (Milliseconds 3000.00) >>= L.as' "let bytes transfer for 3 seconds"

          response2 <- assertStatusCode 200 =<< HTTP.getIngestMetrics E.p1n1
          body2 <- getResBody response2
          let bytes2 = fromMaybe 0 $ getValue $ getLine 5 body2

          Assert.assert "bytes_received aren't increasing" (bytes1 < bytes2)
            >>= L.as' ("byte1: " <> show bytes1 <> ", byte2: " <> show bytes2)


egestMetrics :: forall m. Monad m => SpecT Aff Unit m Unit
egestMetrics =
  describe "10.2 Egest metrics" do
    before (F.startSession [E.p1n1] *> F.launch [E.p1n1] *> T.launch options) do
      after (\browser -> (T.close browser *> F.stopSession *> F.stopSlot)) do
        it "10.2.1 egest bytes should increase" $ \browser -> do
          E.waitForAsyncProfileStart >>= L.as' "wait for async start of profile"

          ingestPage <- T.newPage browser
          T.goto (HTTP.ingestUrl E.p1n1 E.shortName1 E.highStreamName) ingestPage
          delay (Milliseconds 2000.00) >>= L.as' "wait for page to load"
          T.bringToFront ingestPage
          T.click (T.Selector "#authenticate") ingestPage
          delay (Milliseconds 500.00) >>= L.as' "wait for authentication"
          T.click (T.Selector "#start-ingest") ingestPage
          delay (Milliseconds 2000.00) >>= L.as' "let stream start"

          delay (Milliseconds 1000.00) >>= L.as' "let ingest start"

          egestPage <- T.newPage browser
          T.goto (F.mkPlayerUrl E.p1n1 E.slot1 Primary) egestPage

          delay (Milliseconds 10000.00) >>= L.as' "let egest start"

          response1 <- assertStatusCode 200 =<< HTTP.getEgestMetrics E.p1n1
          body1 <- getResBody response1
          let videoOctet1 = fromMaybe 0 $ getValue $ getLine 2 body1

          delay (Milliseconds 11000.00) >>= L.as' "let bytes transfer for 11 seconds"

          response2 <- assertStatusCode 200 =<< HTTP.getEgestMetrics E.p1n1
          body2 <- getResBody response2
          let videoOctet2 = fromMaybe 0 $ getValue $ getLine 2 body2

          Assert.assert "video_octets aren't increasing" (videoOctet1 < videoOctet2)
            >>= L.as' ("octets: " <> show videoOctet1 <> ", octets: " <> show videoOctet2)


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------
getResBody :: Either String ResWithBody -> Aff (Either String String)
getResBody response =
  case response of
    Left e -> pure $ Left e
    Right res -> do
      pure $ Right res.body


getLine :: Int -> (Either String String) -> (Either Error String)
getLine i body =
  case body of
    Left err -> throwError $ Ex.error err
    Right b -> do
      case lines b !! i of
        Nothing -> throwError $ Ex.error "this line isn't present"
        Just line -> Right line

getValue :: Either Error String -> Maybe Int
getValue line =
  case line of
    Left err -> Nothing
    Right l ->
     case split (Pattern " ") l !! 1 of
       Nothing -> Nothing
       Just a -> fromString a
