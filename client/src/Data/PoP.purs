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
  , timedRoutedToChartScatter
  , unGeoLoc
  , updatePoPDefEnv
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Array (catMaybes, concat, findMap, index, length, mapMaybe, nub, nubBy)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (overF, un, wrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Global (readFloat)
import Rtsv2App.Capability.Resource.Api (class ManageApi, getServerState)
import Rtsv2App.Env (PoPDefEnv)
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
  , aggrLocs      :: AggregatorLocation Array
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
toPoPleaders = mapMaybe (_ >>= _.currentTransPoPLeader)

toAggregators :: (Array (Maybe (IntraPoP Array))) -> (AggregatorLocation Array)
toAggregators mIntraPoPs =
  nub $ catMaybes mIntraPoPs
  >>= map toAggr <<< _.aggregatorLocations
  where
    toAggr = \{slotId, role, servers} ->
      { slotId
      , role
      , servers: overF wrap (map _.resource) servers
      }
          

timedRoutedToChartOps :: TimedPoPRoutes Array -> Array LeaderGeoLoc -> Array (Array (Array LeaderGeoLoc))
timedRoutedToChartOps timedRoutes leaderGeolocs =
  convertToLeaderGeoLoc <$> timedRoutes.routes
  where
    convertToLeaderGeoLoc =
      map (\route -> do
        [
          { coord: getCoords route.from leaderGeolocs
          , name: route.from
          }
        , { coord: getCoords route.to leaderGeolocs
          , name: route.to
          }
        ]
      )

timedRoutedToChartScatter :: TimedPoPRoutes Array -> (Array LeaderGeoLoc) -> Array PoPDefEcharts
timedRoutedToChartScatter timedRoutes leaderGeolocs =
  nubBy (comparing _.name) <<< concat $ convertToLeaderGeoLoc <$> timedRoutes.routes
  where
    convertToLeaderGeoLoc routes = do
      let to = map (\route -> { value: getCoords route.to leaderGeolocs , name: route.to}) routes
          from = map (\route -> { value: getCoords route.from leaderGeolocs , name: route.from}) routes
      to <> from


getCoords :: PoPName -> Array LeaderGeoLoc -> Array Number
getCoords name leaderGeolocs =
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
