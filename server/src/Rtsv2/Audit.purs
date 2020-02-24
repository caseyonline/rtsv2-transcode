module Rtsv2.Audit
       (
         IngestEqLine
       , ingestUpdate
       , ingestStop
       , clientStart
       , clientStop
       ) where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (List, nil, (:))
import Logger as Logger
import Shared.LlnwApiTypes (StreamIngestProtocol(..))
import Shared.Stream (EgestKey(..), SlotId(..), SlotRole)
import Shared.Types (Milliseconds)

foreign import toList :: String -> List Char

type IngestEqLine =
  { ingestIp :: String
  , ingestPort :: Int
  , userIp :: String
  , username :: String
  , rtmpShortName :: String
  , rtmpStreamName :: String
  , slotRole :: SlotRole
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

clientStart :: SlotId -> Effect Unit
clientStart (SlotId slotId) = do
  _ <- Logger.info "" { domain: (atom "audit") : (atom "client") : nil
                      , event: toList "start"
                      , slotId: toList $ show slotId}
  pure unit

clientStop :: EgestKey -> Effect Unit
clientStop (EgestKey (SlotId slotId)) = do
  _ <- Logger.info "" { domain: (atom "audit") : (atom "client") : nil
                      , event: toList "stop"
                      , slotId: toList $ show slotId}
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
                 , shortname: toList rtmpShortName
                 , streamName: toList rtmpStreamName
                 , streamRole: toList $ show slotRole
                 , connectionType: toList $ case connectionType of
                                              Rtmp -> "RTMP"
                                              WebRTC -> "WebRTC"
                 , startMs
                 , endMs
                 , bytesWritten
                 , bytesRead
                 , lostPackets
                 }
