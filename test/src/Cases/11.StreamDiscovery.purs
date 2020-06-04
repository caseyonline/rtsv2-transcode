module Cases.StreamDiscovery (streamDiscoveryTests) where

import Prelude

import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import Debug.Trace (spy)
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
import Test.Spec (SpecT, after_, before_, describe, describeOnly, it, itOnly)
import Test.Unit.Assert as Assert
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
  describeOnly "10.1 Ingest metrics" do
    before_ (F.startSession [E.p1n1] *> F.launch [E.p1n1]) do
      after_ (F.stopSession *> F.stopSlot) do
        it "10.1.1 ingest bytes should increase " do

          response <- assertStatusCode 200 =<< HTTP.getStreamDiscovery E.p1n1 E.shortName1 E.highStreamName
          body1 <- getResBody response
          let b1 = getLine 1 body1
          _ <- delay (Milliseconds 3000.00) >>= L.as' "let bytes transfer for 3 seconds"


          F.startSlotHigh1000 (C.toAddrFromNode E.p1n1)
          E.waitForAsyncProfileStart >>= L.as' "wait for async start of profile"

          response2 <- assertStatusCode 200 =<< HTTP.getStreamDiscovery E.p1n1 E.shortName1 E.highStreamName
          body2 <- getResBody response2

          case body2 of
            Left err -> do
              pure unit
            Right b -> pure unit

          pure unit

          -- Assert.assert "bytes_received aren't increasing" (b1 == b2)
          --   >>= L.as' ("byte1: " <> b1 <> ", byte2: " <> b2)


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------
getResBody :: Either String ResWithBody -> Aff (Either String String)
getResBody response =
  case response of
    Left e -> pure $ Left $ spy "err" e
    Right res -> do
      pure $ Right $ spy "body" res.body


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
