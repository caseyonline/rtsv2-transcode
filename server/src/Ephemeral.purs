module Ephemeral
       ( EData
       , extract
       , createdAt
       , garbageCollect
       , new
       , new'
       , updateTimestamp
       , updateTimestamp'
       , expired
       ) where

import Prelude

import Data.Function (on)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Shared.Common (Milliseconds)
import Erl.Utils as Erl

data EData a = EData Milliseconds a

instance eqEData :: Eq a => Eq (EData a) where
  eq = ephemeralEqual

ephemeralEqual :: forall a. Eq a => EData a -> EData a -> Boolean
ephemeralEqual = eq `on` extract

new :: forall a. Milliseconds -> a -> EData a
new = EData

new' :: forall a. a -> Effect (EData a)
new' a = do
  now <- Erl.systemTimeMs
  pure $ new now a

extract :: forall a. EData a -> a
extract (EData _ a) =  a

createdAt :: forall a. EData a -> Milliseconds
createdAt (EData ms _) =  ms

updateTimestamp :: forall a. EData a -> Milliseconds -> EData a
updateTimestamp (EData _ a) ms = EData ms a

updateTimestamp' :: forall a. EData a -> Effect (EData a)
updateTimestamp' edata = do
  now <- Erl.systemTimeMs
  pure $ updateTimestamp edata now

garbageCollect :: forall a. Milliseconds -> EData a -> Maybe (EData a)
garbageCollect threshold ed =
  if expired threshold ed then Nothing
  else Just ed

expired :: forall a. Milliseconds -> EData a -> Boolean
expired threshold ed@(EData ms _) = threshold > ms
