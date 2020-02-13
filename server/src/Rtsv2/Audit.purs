module Rtsv2.Audit
       (
         ingestStart
       , ingestStop
       , clientStart
       , clientStop
       ) where

import Prelude

import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (List, nil, (:))
import Logger as Logger
import Shared.Stream (IngestKey(..), StreamAndVariant(..), StreamId(..))

foreign import toList :: String -> List Char

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

clientStart :: StreamId -> Effect Unit
clientStart (StreamId streamId) = do
  _ <- Logger.info "" { domain: (atom "audit") : (atom "client") : nil
                      , event: toList "start"
                      , streamId: toList streamId}
  pure unit

clientStop :: StreamId -> Effect Unit
clientStop (StreamId streamId) = do
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
