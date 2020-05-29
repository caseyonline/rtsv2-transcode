module Rtsv2.Config where

import Prelude

import Data.Int as Int
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Shared.Rtsv2.Types (WebConfig)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window as Window

-- Web config stub, this is used in URL construction in the endpoint modules
-- However the support client only needs the support port, which is its origin port
webConfig :: Effect WebConfig
webConfig = do
  originPortStr <- window >>= Window.location >>= L.port
  let originPort = fromMaybe 80 $ Int.fromString originPortStr
  pure { publicPort: unsafeCoerce "This should not be used"
       , systemPort: unsafeCoerce "This should not be used"
       , supportPort: originPort
       }
