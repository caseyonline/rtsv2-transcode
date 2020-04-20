module Helpers.Env where

import Prelude

import Data.Array (catMaybes, filter, intercalate)
import Data.Enum (class BoundedEnum, fromEnum)
import Data.Map as Map
import Data.Maybe (Maybe(..),  fromMaybe')
import Data.Newtype (class Newtype, un, wrap, unwrap)
import Data.String as String
import Data.Time (Time(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Now as Now
import Helpers.Types (Node(..), TestNode)
import OsCmd (runProc)
import Shared.Chaos as Chaos
import Shared.Stream (ProfileName(..), SlotId, SlotRole(..), SlotNameAndProfileName(..))
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


-- | Slot
slot1 = wrap (fromMaybe' (lazyCrashIfMissing "Invalid UUID") (fromString "00000000-0000-0000-0000-000000000001"))

shortName1 = wrap "mmddev001"

low = SlotNameAndProfileName "slot1" (wrap "500")

high = SlotNameAndProfileName "slot1" (wrap "1000")


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
waitForIntraPoPDisseminate = delayMs  700.0

waitForNodeStartDisseminate :: Aff Unit
waitForNodeStartDisseminate = delayMs 1000.0

waitForNodeFailureDisseminate :: Aff Unit
waitForNodeFailureDisseminate = delayMs 750.0

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
