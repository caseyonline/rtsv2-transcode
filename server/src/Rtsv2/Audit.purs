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
import Shared.Stream (EgestKey(..), StreamId(..))
import Shared.Types (Milliseconds)

foreign import toList :: String -> List Char

type IngestEqLine =
  { ingestIp :: String
  , ingestPort :: Int
  , userIp :: String
  , username :: String
  , shortname :: String
  , streamName :: String
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

clientStart :: StreamId -> Effect Unit
clientStart (StreamId streamId) = do
  _ <- Logger.info "" { domain: (atom "audit") : (atom "client") : nil
                      , event: toList "start"
                      , streamId: toList streamId}
  pure unit

clientStop :: EgestKey -> Effect Unit
clientStop (EgestKey (StreamId streamId)) = do
  _ <- Logger.info "" { domain: (atom "audit") : (atom "client") : nil
                      , event: toList "stop"
                      , streamId: toList streamId}
  pure unit


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
writeIngestLine :: String -> IngestEqLine -> Effect Unit
writeIngestLine reason { ingestIp
                       , ingestPort
                       , userIp
                       , username
                       , shortname
                       , streamName
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
                 , shortname: toList shortname
                 , streamName: toList streamName
                 , connectionType: toList $ case connectionType of
                                              Rtmp -> "RTMP"
                                              WebRTC -> "WebRTC"
                 , startMs
                 , endMs
                 , bytesWritten
                 , bytesRead
                 , lostPackets
                 }
