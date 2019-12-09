module Ephemeral
       ( EData
       , extract
       , createdAt
       , garbageCollect
       , new
       , expired
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Erl.Utils (Milliseconds)



data EData a = EData Milliseconds a


new :: forall a. Milliseconds-> a -> EData a 
new = EData

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
