module Rtsv2App.Data.PoP
  ( PoPDefEcharts(..)
  , getPoPDefEchart
  , getPoPServers
  , getPoPState
  , getPoPLeaderAddress
  , toPoPEcharts
  , toAggregators
  , toPoPleaders
  , timedRoutedToChartOps
  , unGeoLoc
  , updatePoPDefEnv
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Array (catMaybes, findMap, index, length, mapMaybe, nub)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un, wrap, unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Global (readFloat)
import Rtsv2App.Capability.Resource.Api (class ManageApi, getServerState)
import Rtsv2App.Env (PoPDefEnv)
import Shared.JsonLd as JsonLd
import Shared.Types (GeoLoc(..), LeaderGeoLoc, PoPName, Server(..), ServerAddress)
import Shared.Types.Agent.State (AggregatorLocation, IntraPoP, PoP, PoPDefinition, TimedPoPRoutes)


type PoPDefEcharts =
  { name  :: PoPName
  , value :: Array Number
  }

type Servers = Array Server

type PoPServer =
  { popName :: PoPName
  , server  :: Maybe ServerAddress
  }

type PoPEnvWithoutRef =
  { popDefenition :: Maybe (PoPDefinition Array)
  , popDefEcharts :: Array PoPDefEcharts
  , popLeaders    :: Array Server
  , aggrLocs      :: Array (AggregatorLocation Array)
  }


-- | Convert PoPDefinition to be used by Echarts to diplay locations of pops
toPoPEcharts :: (PoPDefinition Array) -> Array PoPDefEcharts
toPoPEcharts = getPoPDefEchart <<< getPoPFromRegion

getPoPDefEchart :: Array (PoP Array) -> Array PoPDefEcharts
getPoPDefEchart pa = (\p -> { name: p.name, value: unGeoLoc <$> p.geoLoc } ) <$> pa


-- | Convert PoPDefinition to be used by Echarts to diplay locations of pops
toLeaderGeoLoc :: (PoPDefinition Array) -> Array LeaderGeoLoc
toLeaderGeoLoc = getLeaderGeoLoc <<< getPoPFromRegion

getLeaderGeoLoc :: Array (PoP Array) -> Array LeaderGeoLoc
getLeaderGeoLoc pa = (\p -> { name: p.name, coord: unGeoLoc <$> p.geoLoc } ) <$> pa


-- | grab ut all the PoPs from each region
getPoPFromRegion :: (PoPDefinition Array) -> Array (PoP Array)
getPoPFromRegion pd = (\r -> r.pops) =<< pd.regions

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
getPoPState = traverse (\popServer -> hush <$> getServerState popServer.server)

getPoPLeaderAddress :: Array Server -> Maybe PoPName -> Maybe ServerAddress
getPoPLeaderAddress popLeaders popName =
  case popName of
    Nothing    -> Nothing
    Just pName -> findMap (\popLeader -> do
                              let pl = un Server popLeader
                              if (pl.pop == pName) then Just pl.address else Nothing) popLeaders

toPoPleaders :: Array (Maybe (IntraPoP Array)) -> Array Server
--toPoPleaders = mapMaybe (_ >>= (JsonLd.unwrapNode <$> _.currentTransPoPLeader))
toPoPleaders = mapMaybe (_ >>= (\{currentTransPoPLeader} -> JsonLd.unwrapNode <$> currentTransPoPLeader))

toAggregators :: (Array (Maybe (IntraPoP Array))) -> Array (AggregatorLocation Array)
toAggregators mIntraPoPs =
  let
    toAggregatorLocation {slotId, role, servers} = {slotId, role, servers: (JsonLd.unwrapNode <$> servers)}
  in
   nub $ catMaybes mIntraPoPs >>= (\{aggregatorLocations} -> toAggregatorLocation <$> aggregatorLocations)

timedRoutedToChartOps :: TimedPoPRoutes Array -> (Array LeaderGeoLoc) -> Array (Array (Array LeaderGeoLoc))
timedRoutedToChartOps timedRoutes leaderGeolocs =
  convertToLeaderGeoLoc <$> timedRoutes.routes
  where
    convertToLeaderGeoLoc routes = do

      map (\route -> do
        [
          { coord: getCoords route.from
          , name: route.from
          }
        , { coord: getCoords route.to
          , name: route.to
          }
        ]
      ) routes

    getCoords :: PoPName -> Array Number
    getCoords name =
      fromMaybe [0.00, 0.00] $ findMap (\leader -> if name == leader.name
                                                   then Just leader.coord
                                                   else Nothing) leaderGeolocs

updatePoPDefEnv
  :: forall m r
   . MonadAff m
  => MonadAsk { popDefEnv :: PoPDefEnv | r } m
  => ManageApi m
  => PoPDefinition Array
  -> m PoPEnvWithoutRef
updatePoPDefEnv pd = do
  { popDefEnv } <- ask
  popServers <- liftEffect $ getPoPServers pd
  popStates <- getPoPState popServers

  let popDefenition = Just pd
      popDefEcharts = toPoPEcharts pd
      popLeaders    = toPoPleaders popStates
      aggrLocs      = toAggregators popStates
      geoLocs       = toLeaderGeoLoc pd

  liftEffect do
    Ref.write popDefenition popDefEnv.popDefinition
    Ref.write popLeaders popDefEnv.transPoPLeaders
    Ref.write aggrLocs popDefEnv.aggregatorLocations
    Ref.write geoLocs popDefEnv.geoLocations

  pure { popDefenition
       , popDefEcharts
       , popLeaders
       , aggrLocs
       }
