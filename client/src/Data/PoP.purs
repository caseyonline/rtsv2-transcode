module Rtsv2App.Data.PoP
  ( PoPDefEcharts(..)
  , getPoPEcharts
  , getPoPInfo
  , toPoPleaders
  , toAggregators
  , getPoPServers
  , getPoPState
  , unGeoLoc
  ) where

import Prelude

import Data.Array (catMaybes, head, index, length)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Random (randomInt)
import Global (readFloat)
import Rtsv2App.Capability.Resource.Api (class ManageApi, getPublicState)
import Shared.Types (GeoLoc(..), PoPName, Server, ServerAddress)
import Shared.Types.Agent.State (IntraPoP, PoP, PoPDefinition, AggregatorLocation)


type PoPDefEcharts =
  { name  :: PoPName
  , value :: Array Number
  }

type PoPLeaders = Array Server

type PoPServer =
  { popName :: PoPName
  , server  :: Maybe ServerAddress
  }

-- | Convert PoPDefinition to be used by Echarts to diplay locations of pops
getPoPEcharts :: (PoPDefinition Array) -> Array PoPDefEcharts
getPoPEcharts = getPoPInfo <<< getPoPFromRegion

-- | grab ut all the PoPs from each region
getPoPFromRegion :: (PoPDefinition Array) -> Array (PoP Array)
getPoPFromRegion pd = (\r -> r.pops) =<< pd.regions

getPoPInfo :: Array (PoP Array) -> Array PoPDefEcharts
getPoPInfo pa = (\p -> { name: p.name, value: unGeoLoc <$> p.geoLoc } ) <$> pa

unGeoLoc :: GeoLoc -> Number
unGeoLoc = readFloat <<< (un GeoLoc)


-- | make an array of all popNames and one random server for each
getPoPServers :: (PoPDefinition Array) -> Effect (Array PoPServer)
getPoPServers =  getServerInfo <<< getPoPFromRegion

getServerInfo :: Array (PoP Array) -> Effect (Array PoPServer)
getServerInfo pa = traverse
  (\p -> do
      server <- getRandomPoPServer p.servers
      pure $ { popName: p.name, server }
  ) pa

getRandomPoPServer :: Array ServerAddress -> Effect (Maybe ServerAddress)
getRandomPoPServer servers = do
  r <- (randomInt 0 (length servers - 1))
  pure $ index servers r

getPoPState :: forall m. ManageApi m => Array PoPServer -> m (Array (Maybe (IntraPoP Array)))
getPoPState = traverse (\popServer -> hush <$> getPublicState popServer.server)

toPoPleaders :: (Array (Maybe (IntraPoP Array))) -> (Array Server)
toPoPleaders =
  catMaybes <<< map (\intraP ->
                      case intraP of
                        Nothing -> Nothing
                        Just i  -> i.currentTransPoPLeader
                    )

toAggregators :: (Array (Maybe (IntraPoP Array))) -> (AggregatorLocation Array)
toAggregators mIntraPoPs =
  case head $ catMaybes mIntraPoPs of
    Nothing -> []
    Just i  -> i.aggregatorLocations
