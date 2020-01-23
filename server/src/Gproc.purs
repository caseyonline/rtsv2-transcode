
module Gproc
       ( isRegistered
       , register
       , match
       )
       where

import Prelude

import Effect (Effect)
import Erl.Data.List (List)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

foreign import registered_ :: forall a. a -> Effect Boolean

foreign import register_ :: forall a. a -> Effect Unit

foreign import match_ :: forall a. a -> Effect (List Foreign)

isRegistered :: forall a. a -> Effect Boolean
isRegistered name = registered_ name

register :: forall a. a -> Effect Unit
register name = do
  _ <- register_ name
  pure unit


match :: forall matchSpec a. matchSpec -> Effect (List a)
match = unsafeCoerce <$> match_
