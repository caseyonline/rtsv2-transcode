module Helpers.Env where

import Prelude

import Data.Array (catMaybes, filter, intercalate)
import Data.Enum (class BoundedEnum, fromEnum)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, un, wrap, unwrap)
import Data.String as String
import Data.Time (Time(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Now as Now
import Helpers.OsCmd (runProc)
import Helpers.Types (Node(..), TestNode)
import Shared.Rtsv2.Chaos as Chaos
import Shared.Rtsv2.Stream (ProfileName(..), RtmpShortName, RtmpStreamName, SlotId, SlotName(..), SlotNameAndProfileName(..), SlotRole(..))
import Shared.UUID (fromString)
import Shared.Utils (lazyCrashIfMissing)

-- | Node
p1n1 = Node 1 1
p1n2 = Node 1 2
p1n3 = Node 1 3
p2n1 = Node 2 1
p2n2 = Node 2 2
p3n1 = Node 3 1
p3n2 = Node 3 2
p4n1 = Node 4 1
p4n2 = Node 4 2

sessionName:: String
sessionName = "testSession"

stop :: String
stop = "stop"

-- | Chrome launch arguments
browserLaunchArgs :: Array String
browserLaunchArgs =
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
browserLaunchArgsIng :: Array String
browserLaunchArgsIng =
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


-- | Slot
slot1 :: SlotId
slot1 = wrap (fromMaybe' (lazyCrashIfMissing "Invalid UUID") (fromString "00000000-0000-0000-0000-000000000001"))

slot2 :: SlotId
slot2 = wrap (fromMaybe' (lazyCrashIfMissing "Invalid UUID") (fromString "00000000-0000-0000-0000-000000000002"))

slot1Name :: SlotName
slot1Name = wrap "slot1"

shortName1 :: RtmpShortName
shortName1 = wrap "mmddev001"

lowStreamName :: RtmpStreamName
lowStreamName = wrap "slot1_500"

highStreamName :: RtmpStreamName
highStreamName = wrap "slot1_1000"

lowSlotAndProfileName = SlotNameAndProfileName "slot1" (wrap "low")
highSlotAndProfileName = SlotNameAndProfileName "slot1" (wrap "high")

lowProfileName :: ProfileName
lowProfileName = wrap "low"

highProfileName :: ProfileName
highProfileName = wrap "high"

-- | Ingest
ingestAggregatorName slotId role =
  Chaos.Gproc (Chaos.GprocTuple2
                (Chaos.String "IngestAggregator")
                (Chaos.GprocTuple3
                 (Chaos.Atom "aggregatorKey")
                 (Chaos.SlotId slotId)
                 (Chaos.SlotRole role)))

ingestName slotId role streamName =
  Chaos.Gproc (Chaos.GprocTuple2
                (Chaos.String "Ingest")
                (Chaos.GprocTuple4
                   (Chaos.Atom "ingestKey")
                   (Chaos.SlotId slotId)
                   (Chaos.SlotRole role)
                   (Chaos.String streamName)))


-- | Timings & Delays
delayMs = delay <<< Milliseconds

waitForMessageTimeout :: Aff Unit
waitForMessageTimeout = delayMs 2000.0

waitForSupervisorRecovery :: Aff Unit
waitForSupervisorRecovery = delayMs 100.0

waitForAsyncProfileStart :: Aff Unit
waitForAsyncProfileStart = delayMs 250.0 -- Time between starting an ingest and the aggregator being up with that profile

waitForAsyncProfileStop :: Aff Unit
waitForAsyncProfileStop = delayMs  250.0 -- Time between stopping an ingest and the aggregator removing that profile

waitForAsyncRelayStart :: Aff Unit
waitForAsyncRelayStart = delayMs  200.0

waitForAsyncRelayStop :: Aff Unit
waitForAsyncRelayStop = delayMs  100.0

waitForIntraPoPDisseminate :: Aff Unit
waitForIntraPoPDisseminate = delayMs 700.0

waitForNodeStartDisseminate :: Aff Unit
waitForNodeStartDisseminate = delayMs 1000.0

waitForNodeFailureDisseminate :: Aff Unit
waitForNodeFailureDisseminate = delayMs 1000.0

waitForTransPoPDisseminate :: Aff Unit
waitForTransPoPDisseminate = delayMs 2000.0

waitForTransPoPStopDisseminate :: Aff Unit
waitForTransPoPStopDisseminate = delayMs 5000.0 -- TODO - seeems big

waitForLessThanLinger :: Aff Unit
waitForLessThanLinger = delayMs  500.0

waitForMoreThanLinger :: Aff Unit
waitForMoreThanLinger = delayMs 1500.0

waitForMoreThanEgestLinger :: Aff Unit
waitForMoreThanEgestLinger = delayMs 3000.0 -- TODO seems big

waitForHalfLoadDecay :: Aff Unit
waitForHalfLoadDecay = delayMs 5000.0

waitForLoadTick :: Aff Unit
waitForLoadTick = delayMs 2000.0

currentTime :: Effect String
currentTime = do
  Time hour minute second millisecond <- Now.nowTime
  pure $ (format hour 2)
         <> ":"
         <> (format minute 2)
         <> ":"
         <> (format second 2)
         <> "."
         <> (format millisecond 3)
  where
    format :: forall a. BoundedEnum a => a -> Int -> String
    format enum len = do
      let out = show (fromEnum enum)
          pad = len - (String.length out)
      if pad == 1 then "0" <> out
        else if pad == 2 then "00" <> out
          else out
