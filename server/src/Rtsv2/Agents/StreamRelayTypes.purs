module Rtsv2.Agents.StreamRelayTypes
  ( CreateRelayPayload
  , AggregatorPrimaryToBackupWsMessage(..)
  , AggregatorBackupToPrimaryWsMessage(..)
  , RelayUpstreamWsMessage(..)
  , EgestUpstreamWsMessage(..)
  , IngestToAggregatorWsMessage(..)
  , AggregatorToIngestWsMessage(..)
  , DownstreamWsMessage(..)
  , WebSocketHandlerMessage(..)
  , ActiveProfiles(..)
  , NativeJson
  ) where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Erl.Data.List (List)
import Erl.Utils (Ref)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.DataObject (class DataObjectRef)
import Rtsv2.DataObject as DO
import Shared.Common (Milliseconds)
import Shared.Rtsv2.Agent (SlotCharacteristics)
import Shared.Rtsv2.Stream (ProfileName, SlotId, SlotRole)
import Shared.Rtsv2.Types (PoPName)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Unsafe.Coerce (unsafeCoerce)

foreign import data NativeJson :: Type

type CreateRelayPayload
  = { slotId :: SlotId
    , slotRole :: SlotRole
    , aggregatorPoP :: PoPName
    , slotCharacteristics :: SlotCharacteristics
    }

data IngestToAggregatorWsMessage = IngestToAggregatorDataObjectMessage DO.Message
                                 | IngestToAggregatorDataObjectUpdateMessage DO.ObjectUpdateMessage

data AggregatorToIngestWsMessage = IngestStop
                                 | AggregatorToIngestDataObjectMessage DO.Message
                                 | AggregatorToIngestDataObjectUpdateResponse DO.ObjectUpdateResponseMessage
                                 | AggregatorToIngestDataObject DO.Object

data AggregatorPrimaryToBackupWsMessage = P2B_Synchronise
                                        | P2B_Latest DO.Object
                                        | P2B_Message DO.Message
                                        | P2B_UpdateResponse DO.ObjectUpdateResponseMessage
                                        | P2B_ClientCount Int

data AggregatorBackupToPrimaryWsMessage = B2P_SynchroniseObject DO.Object
                                        | B2P_SynchroniseNoObject
                                        | B2P_Message DO.Message
                                        | B2P_Update DO.ObjectUpdateMessage
                                        | B2P_ClientCount Int

data RelayUpstreamWsMessage = RelayUpstreamDataObjectMessage DO.Message
                            | RelayUpstreamDataObjectUpdateMessage DO.ObjectUpdateMessage
                            | RelayAggregateClientCount {count :: Int, time :: Milliseconds}

data EgestUpstreamWsMessage = EdgeToRelayDataObjectMessage DO.Message
                            | EdgeToRelayDataObjectUpdateMessage DO.ObjectUpdateMessage
                            | EgestClientCount Int

newtype ActiveProfiles = ActiveProfiles { profiles :: List ProfileName
                                        , ref :: Ref
                                        }

data DownstreamWsMessage = SlotConfig SlotConfiguration
                         | OnFI {payload :: NativeJson, pts :: Int}
                         | CurrentActiveProfiles ActiveProfiles
                         | DataObjectMessage DO.Message
                         | DataObjectUpdateResponse DO.ObjectUpdateResponseMessage
                         | DataObject DO.ObjectBroadcastMessage
                         | ClientCount Int

data WebSocketHandlerMessage a = WsStop
                               | WsSend a

------------------------------------------------------------------------------
-- ActiveProfiles
derive instance genericActiveProfiles :: Generic ActiveProfiles _

instance readForeignActiveProfiles :: ReadForeign ActiveProfiles where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignActiveProfiles :: WriteForeign ActiveProfiles where
  writeImpl msg = writeImpl (genericSumToVariant msg)

instance dataObjectRefActiveProfiles :: DataObjectRef ActiveProfiles where
  ref (ActiveProfiles {ref: msgRef}) = msgRef

------------------------------------------------------------------------------
-- IngestToAggregatorWsMessage
derive instance genericIngestToAggregatorWsMessage :: Generic IngestToAggregatorWsMessage _

instance readForeignIngestToAggregatorWsMessage :: ReadForeign IngestToAggregatorWsMessage where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignIngestToAggregatorWsMessage :: WriteForeign IngestToAggregatorWsMessage where
  writeImpl msg = writeImpl (genericSumToVariant msg)


------------------------------------------------------------------------------
-- AggregatorToIngestWsMessage
derive instance genericAggregatorToIngestWsMessage :: Generic AggregatorToIngestWsMessage _

instance readForeignAggregatorToIngestWsMessage :: ReadForeign AggregatorToIngestWsMessage where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignAggregatorToIngestWsMessage :: WriteForeign AggregatorToIngestWsMessage where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- AggregatorPrimaryToBackupWsMessage
derive instance genericAggregatorPrimaryToBackupWsMessage :: Generic AggregatorPrimaryToBackupWsMessage _

instance readForeignAggregatorPrimaryToBackupWsMessage :: ReadForeign AggregatorPrimaryToBackupWsMessage where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignAggregatorPrimaryToBackupWsMessage :: WriteForeign AggregatorPrimaryToBackupWsMessage where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- AggregatorBackupToPrimaryWsMessage
derive instance genericAggregatorBackupToPrimaryWsMessage :: Generic AggregatorBackupToPrimaryWsMessage _

instance readForeignAggregatorBackupToPrimaryWsMessage :: ReadForeign AggregatorBackupToPrimaryWsMessage where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignAggregatorBackupToPrimaryWsMessage :: WriteForeign AggregatorBackupToPrimaryWsMessage where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- RelayUpstreamWsMessage
derive instance genericRelayUpstreamWsMessage :: Generic RelayUpstreamWsMessage _

instance readForeignRelayUpstreamWsMessage :: ReadForeign RelayUpstreamWsMessage where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignRelayUpstreamWsMessage :: WriteForeign RelayUpstreamWsMessage where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- EgestUpstreamWsMessage
derive instance genericEgestUpstreamWsMessage :: Generic EgestUpstreamWsMessage _

instance readForeignEgestUpstreamWsMessage :: ReadForeign EgestUpstreamWsMessage where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignEgestUpstreamWsMessage :: WriteForeign EgestUpstreamWsMessage where
  writeImpl msg = writeImpl (genericSumToVariant msg)

------------------------------------------------------------------------------
-- DownstreamWsMessage
derive instance genericDownstreamWsMessage :: Generic DownstreamWsMessage _

instance readForeignDownstreamWsMessage :: ReadForeign DownstreamWsMessage where
  readImpl o = variantToGenericSum <$> readImpl o

instance writeForeignDownstreamWsMessage :: WriteForeign DownstreamWsMessage where
  writeImpl msg = writeImpl (genericSumToVariant msg)


------------------------------------------------------------------------------
-- NativeJson
instance readForeignNativeJson :: ReadForeign NativeJson where
  readImpl o = except (Right (unsafeCoerce o))

instance writeForeignNativeJson :: WriteForeign NativeJson where
  writeImpl = unsafeCoerce
