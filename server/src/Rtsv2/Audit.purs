module Rtsv2.Audit
       (
         ingestStart
       , ingestStop
       , clientStart
       , clientStop
       ) where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (List, nil, (:))
import Logger as Logger
import Shared.Stream (StreamId(..), StreamVariantId(..))

foreign import toList :: String -> List Char

ingestStart :: StreamVariantId -> Effect Unit
ingestStart (StreamVariantId streamId streamVariantId) = do
  _ <- Logger.info "" { domain: (atom "audit") : (atom "ingest") : nil
                      , event: toList "start"
                      , customerId: toList "customerId" -- todo
                      , streamId: toList streamId
                      , streamVariantId: toList streamVariantId}
  pure unit

ingestStop :: StreamVariantId -> Effect Unit
ingestStop (StreamVariantId streamId streamVariantId) = do
  _ <- Logger.info "" { domain: (atom "audit") : (atom "ingest") : nil
                      , event: toList "stop"
                      , customerId: toList "customerId" -- todo
                      , streamId: toList streamId
                      , streamVariantId: toList streamVariantId}
  pure unit

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
