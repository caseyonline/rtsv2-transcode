module Ephemeral
       ( EData
       , extract
       , createdAt
       , garbageCollect
       , new
       , new'
       , expired
       ) where

import Prelude

import Data.Function (on)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Shared.Types (Milliseconds)
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

garbageCollect :: forall a. Milliseconds -> EData a -> Maybe (EData a)
garbageCollect threshold ed =
  if expired threshold ed then Nothing
  else Just ed

expired :: forall a. Milliseconds -> EData a -> Boolean
expired threshold ed@(EData ms _) = threshold > ms
