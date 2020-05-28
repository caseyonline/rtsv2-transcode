module Rtsv2.Config where

import Prelude

import Effect (Effect)
import Shared.Rtsv2.Types (WebConfig)

-- these need to match what they are in release-files/rtsv2_core.config
webConfig :: Effect WebConfig
webConfig = pure  { publicPort: 3000
                  , systemPort: 3001
                  , supportPort: 3002
                  }
