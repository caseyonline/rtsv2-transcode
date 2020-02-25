module Rtsv2App.Data.PoP
  ( PoPDefEcharts(..)
  , getPoPEcharts
  , getPoPInfo
  , toPoPleaders
  , toAggregators
  , getPoPServers
  , getPoPState
  , unGeoLoc
  , updatePoPDefEnv
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Array (catMaybes, head, index, length, mapMaybe, nub)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Global (readFloat)
import Rtsv2App.Capability.Resource.Api (class ManageApi, getPublicState)
import Rtsv2App.Capability.Resource.User (class ManageUser)
import Rtsv2App.Env (PoPDefEnv)
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

type PoPEnvNoRef =
  { popDefenition       :: Maybe (PoPDefinition Array)
  , popDefEcharts       :: Array PoPDefEcharts
  , popLeaders          :: Array Server
  , aggregatorLocations :: AggregatorLocation Array
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
toPoPleaders = mapMaybe (_ >>= _.currentTransPoPLeader)

toAggregators :: (Array (Maybe (IntraPoP Array))) -> (AggregatorLocation Array)
toAggregators mIntraPoPs = nub $ catMaybes mIntraPoPs >>= _.aggregatorLocations

updatePoPDefEnv
  :: forall m r
   . MonadAff m
  => MonadAsk { popDefEnv :: PoPDefEnv | r } m
  => ManageApi m
  => PoPDefinition Array
  -> m PoPEnvNoRef
updatePoPDefEnv pd = do
  { popDefEnv } <- ask
  popServers <- liftEffect $ getPoPServers pd
  popStates <- getPoPState popServers

  let popDefenition       = Just pd
      popDefEcharts       = getPoPEcharts pd
      popLeaders          = toPoPleaders popStates
      aggregatorLocations = toAggregators popStates

  liftEffect do
    Ref.write popDefenition popDefEnv.popDefinition
    Ref.write popLeaders popDefEnv.transPoPLeaders
    Ref.write aggregatorLocations popDefEnv.aggregatorLocations

  pure { popDefenition
       , popDefEcharts
       , popLeaders
       , aggregatorLocations
       }
