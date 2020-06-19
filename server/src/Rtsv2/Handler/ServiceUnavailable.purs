module Rtsv2.Handler.ServiceUnavailable
       (
         unavailable
       ) where

import Prelude

import Stetson (StetsonHandler)
import Stetson.Rest as Rest

unavailable :: forall a. Monoid a => StetsonHandler a
unavailable =
    Rest.handler (\req -> Rest.initResult req mempty)
  # Rest.serviceAvailable (\req state -> Rest.result false req state)
  # Rest.yeeha
