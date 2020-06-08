module Gproc
       ( isRegistered
       , register
       , whereIs
       , match
       )
       where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

foreign import registered_ :: forall a. a -> Effect Boolean

foreign import register_ :: forall a. a -> Effect Unit

foreign import whereis_ :: forall a. a -> Effect (Either Unit Pid)

foreign import match_ :: forall a. a -> Effect (List Foreign)

isRegistered :: forall a. a -> Effect Boolean
isRegistered = registered_

register :: forall a. a -> Effect Unit
register = register_

whereIs :: forall a. a -> Effect (Either Unit Pid)
whereIs = whereis_

match :: forall matchSpec a. matchSpec -> Effect (List a)
match = unsafeCoerce <$> match_
