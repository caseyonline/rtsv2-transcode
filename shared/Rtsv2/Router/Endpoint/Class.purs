module Shared.Rtsv2.Router.Endpoint.Class where

import Routing.Duplex (RouteDuplex')
import Shared.Rtsv2.Router.Endpoint.Public as Public
import Shared.Rtsv2.Router.Endpoint.Support as Support
import Shared.Rtsv2.Router.Endpoint.System as System

class RoutedEndpoint a where
  getRoute :: a -> RouteDuplex' a

instance supportEndpoint :: RoutedEndpoint Support.Endpoint where
  getRoute _ = Support.endpoint

instance publicEndpoint :: RoutedEndpoint Public.Endpoint where
  getRoute _ = Public.endpoint

instance systemEndpoint :: RoutedEndpoint System.Endpoint where
  getRoute _ = System.endpoint