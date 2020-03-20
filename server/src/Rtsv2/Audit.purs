module Rtsv2.Audit
       (
         IngestEqLine
       , EgestEqLine
       , ingestUpdate
       , ingestStop
       , egestUpdate
       , egestStop
       , clientStart
       , clientStop
       ) where

import Prelude

import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (List, nil, (:))
import Logger as Logger
import Shared.LlnwApiTypes (StreamIngestProtocol(..))
import Shared.Stream (EgestKey(..), SlotId(..), SlotRole, RtmpShortName, RtmpStreamName)
import Shared.Common (Milliseconds)

foreign import toList :: String -> List Char

type IngestEqLine =
  { ingestIp :: String
  , ingestPort :: Int
  , userIp :: String
  , username :: String
  , rtmpShortName :: RtmpShortName
  , rtmpStreamName :: RtmpStreamName
  , slotId :: SlotId
  , slotRole :: SlotRole
  , connectionType :: StreamIngestProtocol
  , startMs :: Milliseconds
  , endMs :: Milliseconds
  , bytesWritten :: Int
  , bytesRead :: Int
  , lostPackets :: Int
  }

type EgestEqLine =
  { egestIp :: String
  , egestPort :: Int
  , subscriberIp :: String
  , username :: String
  , slotId :: SlotId
  , connectionType :: StreamIngestProtocol
  , startMs :: Milliseconds
  , endMs :: Milliseconds
  , bytesWritten :: Int
  , bytesRead :: Int
  , lostPackets :: Int
  }

ingestUpdate :: IngestEqLine -> Effect Unit
ingestUpdate line =
  writeIngestLine "update" line

ingestStop :: IngestEqLine -> Effect Unit
ingestStop line =
  writeIngestLine "stop" line

egestUpdate :: EgestEqLine -> Effect Unit
egestUpdate line =
  writeEgestLine "update" line

egestStop :: EgestEqLine -> Effect Unit
egestStop line =
  writeEgestLine "stop" line

clientStart :: SlotId -> Effect Unit
clientStart (SlotId slotId) = do
  _ <- Logger.info "" { domain: (atom "audit") : (atom "client") : nil
                      , event: toList "start"
                      , slotId: toList $ show slotId}
  pure unit

clientStop :: EgestKey -> Effect Unit
clientStop (EgestKey (SlotId slotId) slotRole) = do
  _ <- Logger.info "" { domain: (atom "audit") : (atom "client") : nil
                      , event: toList "stop"
                      , slotId: toList $ show slotId
                      , slotRole: toList $ show slotRole
                      }
  pure unit


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
writeIngestLine :: String -> IngestEqLine -> Effect Unit
writeIngestLine reason { ingestIp
                       , ingestPort
                       , userIp
                       , username
                       , rtmpShortName
                       , rtmpStreamName
                       , slotId
                       , slotRole
                       , connectionType
                       , startMs
                       , endMs
                       , bytesWritten
                       , bytesRead
                       , lostPackets
                       } =
  Logger.info "" { domain: (atom "audit") : (atom "ingest") : nil
                 , event: toList reason
                 , ingestIp: toList ingestIp
                 , ingestPort: ingestPort
                 , userIp: toList userIp
                 , username: toList username
                 , shortname: toList $ unwrap rtmpShortName
                 , streamName: toList $ unwrap rtmpStreamName
                 , slotId: toList $ show $ unwrap slotId
                 , slotRole: toList $ show slotRole
                 , connectionType: toList $ case connectionType of
                                              Rtmp -> "RTMP"
                                              WebRTC -> "WebRTC"
                 , startMs
                 , endMs
                 , bytesWritten
                 , bytesRead
                 , lostPackets
                 }

writeEgestLine :: String -> EgestEqLine -> Effect Unit
writeEgestLine reason { egestIp
                      , egestPort
                      , subscriberIp
                      , username
                      , slotId
                      , connectionType
                      , startMs
                      , endMs
                      , bytesWritten
                      , bytesRead
                      , lostPackets
                      } =
  Logger.info "" { domain: (atom "audit") : (atom "egest") : nil
                 , event: toList reason
                 , egestIp: toList egestIp
                 , egestPort: egestPort
                 , subscriberIp: toList subscriberIp
                 , username: toList username
                 , slotId: toList $ show $ unwrap slotId
                 , connectionType: toList $ case connectionType of
                                              Rtmp -> "RTMP"
                                              WebRTC -> "WebRTC"
                 , startMs
                 , endMs
                 , bytesWritten
                 , bytesRead
                 , lostPackets
                 }
