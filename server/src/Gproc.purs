-- -*- psc-ide-codegen: ("erl") -*-
module Gproc
       ( isRegistered
       , register
       )
       where

import Prelude

import Effect (Effect)

foreign import registered_ :: forall a. a -> Effect Boolean

foreign import register_ :: forall a. a -> Effect Unit

isRegistered :: forall a. a -> Effect Boolean
isRegistered name = registered_ name

register :: forall a. a -> Effect Unit
register name = do
  _ <- register_ name
  pure unit
