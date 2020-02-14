module Rtsv2.Audit
       (
         ingestStart
       , ingestStop
       , ingestUpdate
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
import Shared.Stream (EgestKey(..), IngestKey(..), StreamAndVariant(..), StreamId(..))

foreign import toList :: String -> List Char

-- origin ip - StreamPublish
-- origin port - ingestState
-- user ip - RemoteIp
-- username - StreamPublish
-- shortname - StreamPublish
-- streamname - StreamPublish
-- connectiontype - StreamPublish
-- start_ms - ingeststate
-- end_ms - ingeststate
-- bytes written - ingeststate
-- bytes read - ingeststate
-- lost packets - ingeststate

ingestStart :: IngestKey -> Effect Unit
ingestStart (IngestKey streamId streamRole streamVariant) = do
  Logger.info "" { domain: (atom "audit") : (atom "ingest") : nil
                 , event: toList "start"
                 , customerId: toList "customerId" -- todo
                 , streamId: toList $ unwrap streamId
                 , streamVariantId: toList $ unwrap streamVariant
                 , streamRole -- TODO - the rest are strings - not sure what the goal is...
                 }

ingestStop :: IngestKey -> Effect Unit
ingestStop (IngestKey streamId streamRole streamVariant) = do
  Logger.info "" { domain: (atom "audit") : (atom "ingest") : nil
                 , event: toList "stop"
                 , customerId: toList "customerId" -- todo
                 , streamId: toList $ unwrap streamId
                 , streamVariantId: toList $ unwrap streamVariant
                 , streamRole -- TODO - the rest are strings - not sure what the goal is...
                 }

ingestUpdate { ingestIp
             , ingestPort
             , userIp
             , username
             , shortname
             , streamName
             , connectionType
             , startMs
             , endMs} =
  Logger.info "" { domain: (atom "audit") : (atom "ingest") : nil
                 , event: toList "update"
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
                 }


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
-- TODO - agree structured log format
-- domains :: List Atom
-- domains = atom <$> (show Agent.StreamRelay :  "Instance" : nil)

-- logInfo :: forall a. Logger a
-- logInfo = domainLog Logger.info

-- --logWarning :: forall a. Logger a
-- --logWarning = domainLog Logger.warning

-- domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
-- domainLog = Logger.doLog domains
