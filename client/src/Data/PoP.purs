module Rtsv2App.Data.PoP
  ( PoPDefEcharts(..)
  , getPoPEcharts
  , getPoPInfo
  , unGeoLoc
  ) where

import Prelude

import Data.Newtype (un)
import Global (readFloat)
import Shared.Types (GeoLoc(..), PoPName)
import Shared.Types.Agent.State (PoPDefinition, PoP)


type PoPDefEcharts =
  { name  :: PoPName
  , value :: Array Number
  }

-- | Convert PoPDefinition to be used by Echarts to diplay locations of pops
getPoPEcharts :: (PoPDefinition Array) -> Array PoPDefEcharts
getPoPEcharts pd = do
  getPoPInfo $ (\r -> r.pops) =<< pd.regions

getPoPInfo :: Array (PoP Array) -> Array PoPDefEcharts
getPoPInfo pa = (\p -> { name: p.name, value: unGeoLoc <$> p.geoLoc } ) <$> pa

unGeoLoc :: GeoLoc -> Number
unGeoLoc = readFloat <<< (un GeoLoc)

-- need to take popDefinition
-- grab a random ip from "servers" to get back currentTransPoPLeader
-- then attach the two together to be able to make call to correct pop leaders given popname
-- create { popName, currentTransPoPLeader  }
