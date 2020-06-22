module Cases.RtmpEgest where

import Prelude

import Control.Monad.State (evalStateT, lift)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (either)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as Number
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect.Aff (Aff, Milliseconds(..), apathize, attempt, delay)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Helpers.Assert as A
import Helpers.CreateString as C
import Helpers.Env as E
import Helpers.Functions (startSession, launch, stopSession, launch', forceGetState, storeHeader, getStateValue)
import Helpers.Functions as F
import Helpers.HTTP as HTTP
import Helpers.Log (as, as', asT)
import Helpers.Log as L
import Helpers.OsCmd (runProc)
import Node.Buffer as Buffer
import Node.ChildProcess (defaultExecSyncOptions, exec, execSync)
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Partial.Unsafe (unsafePartial)
import Simple.JSON as SimpleJSON
import Test.Spec (SpecT, after_, before_, describe, describeOnly, it, itOnly)
import Test.Spec.Assertions (fail, shouldEqual)


type ProbeResults = {
  streams :: Array StreamInfo
}
type StreamInfo = {
  codec_type :: String,
  duration :: String
}

rtmpEgestTests :: forall m. Monad m => SpecT Aff Unit m Unit
rtmpEgestTests = do
  describe "12 RTMP egest tests" do
    before_ (F.startSession [E.p1n1]
                *> F.launch [E.p1n1]
                *> F.startSlotHigh1000 (C.toAddrFromNode E.p1n1)) do
      after_ (F.stopSession *> F.stopSlot) do
        it "test" do
          E.waitForAsyncProfileStart >>= L.as' "wait for async start of profile"
          let tmpFile = "/tmp/rtsv2_test_egest.ts"
          apathize $ FS.unlink tmpFile
          F.startEgestHigh (C.toAddrFromNode E.p1n1) 1937 tmpFile
          delay (Milliseconds 5000.0)

          probeResultsJson <- F.runFFProbe tmpFile
          probeResults :: ProbeResults <- liftEffect $ either (const $ throw "didn't parse probejson") (pure) $ SimpleJSON.readJSON probeResultsJson

          let res0 = unsafePartial $ fromJust $ probeResults.streams !! 0
          let res1 = unsafePartial $ fromJust $ probeResults.streams !! 1

          res0.codec_type `shouldEqual` "video"
          res1.codec_type `shouldEqual` "audio"

          checkDuration res0.duration 5.0 15.0
          checkDuration res1.duration 5.0 15.0

checkDuration :: String -> Number -> Number -> Aff Unit
checkDuration candidate min max=
  case Number.fromString candidate of
    Nothing -> fail "Duration should be a number"
    Just n | n > min && n < max -> pure unit
    Just n -> fail $ "Duration not in range: " <> show n
