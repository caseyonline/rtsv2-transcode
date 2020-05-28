module Rtsv2.PoPDefinition
       ( startLink
       , getPublicPoPDefinition
       , getOtherServersForThisPoP
       , getOtherPoPs
       , getOtherPoPNames
       , getRandomServerInPoP
       , whereIsServer
       , getThisServer
       , neighbourMap
       , serversInThisPoPByAddress
       , PoP
       , Region
       , NeighbourMap
       ) where

import Prelude

import Data.Either (Either(..), note')
import Data.Filterable (filter)
import Data.Foldable (elem, foldl)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Erl.Atom (Atom)
import Erl.Data.List (List, index, length, nil, singleton, (:))
import Erl.Data.Map (Map, mapMaybeWithKey, toUnfoldable)
import Erl.Data.Map as Map
import File as File
import Foreign (ForeignError(..), MultipleErrors)
import Logger (Logger)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName, StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import PintoHelper (doExposeState, exposeState)
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Names as Names
import Shared.Rtsv2.Agent (Agent)
import Shared.Rtsv2.Types (GeoLoc, NetworkKbps, PoPName, RegionName, Server(..), ServerAddress(..), ServerLocation(..), SpecInt, extractPoP, toServerLocation)
import Shared.Rtsv2.Agent.State as PublicState
import Simple.JSON as JSON

type PoPInfo =
  { regions :: Map RegionName Region
  , servers :: Map ServerAddress Server
  , thisLocation :: ServerLocation
  , otherServersInThisPoP :: List Server
  , thisPoP :: PoP
  , otherPoPs :: List PoP
  , neighbourMap :: NeighbourMap
  }

type State =  { config :: Config.PoPDefinitionConfig
              , regions :: Map RegionName Region
              , servers :: Map ServerAddress Server
              , thisServer :: Server
              , otherServersInThisPoP :: List Server
              , otherPoPs :: Map PoPName PoP
              , neighbourMap :: NeighbourMap
              }

type Region = { name :: RegionName
              , pops :: Map PoPName PoP
              }

type PoP = { name :: PoPName
           , geoLoc :: List GeoLoc
           , servers :: List Server
           , neighbours :: List PoPName
           }

type NetworkJson = Map RegionName PoPJson

type PoPJson =  Map PoPName PoPInfoJson

type ServerJson = { address :: ServerAddress
                  , maxCpuCapacity :: SpecInt
                  , maxNetworkCapacity :: NetworkKbps
                  , receiveQueueCount :: Int
                  , transmitQueueCount :: Int
                  , capabilityTags :: Array String
                  , agents :: Array Agent
                  }

type PoPInfoJson = { geoLoc :: (List GeoLoc)
                   , nodes :: (List ServerJson)
                   }

type NeighbourMap =  Map PoPName (List PoPName)

data Msg = Tick

startLink :: Config.PoPDefinitionConfig -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

neighbourMap :: Effect NeighbourMap
neighbourMap = exposeState _.neighbourMap serverName

getPublicPoPDefinition :: Effect (PublicState.PoPDefinition List)
getPublicPoPDefinition =
  exposeState publicPoPDefinition serverName
  where
    publicPoPDefinition :: State -> PublicState.PoPDefinition List
    publicPoPDefinition state@{regions, neighbourMap: neighbours} =
      { regions : (\{name: regionName, pops} -> {name: regionName
                                    , pops: (\{name: popName, geoLoc, servers, neighbours: popNeighbours} ->
                                              {name: popName, geoLoc, servers: (unwrap >>> _.address) <$> servers, neighbours: popNeighbours}
                                            ) <$> Map.values pops
                                    }) <$> Map.values regions
      , neighbourMap : (\(Tuple popName popNeighbours) -> {popName, neighbours: popNeighbours}) <$> toUnfoldable neighbours
      }

getOtherServersForThisPoP :: Effect (List Server)
getOtherServersForThisPoP = exposeState _.otherServersInThisPoP serverName

getOtherPoPs :: Effect (Map PoPName PoP)
getOtherPoPs = exposeState _.otherPoPs serverName

getOtherPoPNames :: Effect (List PoPName)
getOtherPoPNames = exposeState (Map.keys <<< _.otherPoPs) serverName

getThisServer :: Effect Server
getThisServer = exposeState _.thisServer serverName

getRandomServerInPoP :: PoPName -> Effect (Maybe Server)
getRandomServerInPoP popName = doExposeState stateFn serverName
  where
    stateFn state = do
      let
        mPoP = Map.lookup popName state.otherPoPs
      case mPoP of
        Nothing -> pure Nothing
        Just {servers} -> do
          pos <- randomInt 0 ((length servers) -1)
          pure $ index servers pos


whereIsServer :: ServerAddress -> Effect (Maybe Server)
whereIsServer sa = Gen.doCall serverName
             \state -> pure $ CallReply (Map.lookup sa state.servers) state

serversInThisPoPByAddress :: Effect (Map ServerAddress Server)
serversInThisPoPByAddress =
  Gen.doCall serverName
    \state@{thisServer} ->
      let
        thisPoP = extractPoP thisServer
        result
          = state.servers
            # Map.mapMaybe (\server ->
                             if extractPoP server == thisPoP then
                               Just server
                             else
                               Nothing
                                  )
      in
      pure $ CallReply result state


init :: Config.PoPDefinitionConfig -> Effect State
init config = do
  thisServerAddress <- ServerAddress <$> Env.hostname
  eNeighbourMap <- readNeighbourMap config
  nMap <- case eNeighbourMap of
    Left e -> do
      _ <- logWarning "Failed to process WAN definition file" {misc: e}
      unsafeCrashWith "invalid WAN definition file"
    Right r -> pure r

  ePopInfo <- readAndProcessPoPDefinition config thisServerAddress nMap
  Tuple thisServer popInfo <- case ePopInfo of
    Left e -> do
      _ <- logWarning "Failed to process pop definition file" {misc: e}
      unsafeCrashWith "invalid pop definition file"
    Right r -> pure r
  let
      state =
        { config : config
        , regions : popInfo.regions
        , servers : popInfo.servers
        , thisServer
        , otherPoPs : toMap popInfo.otherPoPs
        , otherServersInThisPoP : popInfo.otherServersInThisPoP
        , neighbourMap : popInfo.neighbourMap
        }
  logInfo "PoPDefinition Starting" { thisServer
                                   , otherServers : state.otherServersInThisPoP}
  _ <- Timer.sendAfter serverName 1000 Tick
  pure state

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{thisServer: (Server thisServer)}=
  case msg of
    Tick ->
      do
        eNeighbourMap <- readNeighbourMap state.config
        newState <-  case eNeighbourMap of
          Left e -> do _ <- logWarning "Failed to process WAN definition file" {misc: e}
                       pure state
          Right nMap -> do
            ePopInfo <- readAndProcessPoPDefinition state.config thisServer.address nMap
            case ePopInfo of

              Left e -> do _ <- logWarning "Failed to process pop definition file" {misc: e}
                           pure state
              Right (Tuple _thisServer popInfo@{thisLocation : (ServerLocation newPoPLocation)})
                | newPoPLocation.pop == thisServer.pop  ->
                  pure $ (state { regions = popInfo.regions
                                , servers = popInfo.servers
                                , otherServersInThisPoP = popInfo.otherServersInThisPoP
                                , otherPoPs = toMap popInfo.otherPoPs
                                } :: State)
                | otherwise ->
                    do _ <- logWarning "This node seems to have changed pop - ignoring" { currentLocation: state.thisServer
                                                                                        , filePoP : newPoPLocation
                                                                                        }
                       pure state

        _ <- Timer.sendAfter serverName 1000 Tick
        pure $ CastNoReply newState

--------------------------------------------------------------------------------
-- Internal funcitons
--------------------------------------------------------------------------------
readNeighbourMap :: Config.PoPDefinitionConfig -> Effect (Either MultipleErrors NeighbourMap)
readNeighbourMap config = do
  file <- readFile $ joinWith "/" [config.directory, config.wanDefinitionFile]
  pure $ JSON.readJSON =<< file

readAndProcessPoPDefinition :: Config.PoPDefinitionConfig -> ServerAddress -> NeighbourMap -> Effect (Either MultipleErrors (Tuple Server PoPInfo))
readAndProcessPoPDefinition config thisServerAddress nMap = do
  -- In Effect
  file <- readFile $ joinWith "/" [config.directory, config.popDefinitionFile]
  let ePoPJson = (JSON.readJSON =<< file)
  pure do -- In either
        popJson <- ePoPJson
        let allPops = Map.keys =<< Map.values popJson
            filteredNeighbours = filterNeighbours allPops nMap
            regionMap = mapRegionJson filteredNeighbours popJson
        processPoPJson thisServerAddress regionMap filteredNeighbours

readFile :: String -> Effect (Either MultipleErrors String)
readFile fileName = do
  jsonString <- File.readUtf8File fileName
  pure $ note' (\_ -> NonEmptyList.singleton $ ForeignError ("failed to read file " <> fileName)) jsonString

filterNeighbours :: List PoPName -> NeighbourMap -> NeighbourMap
filterNeighbours allPops nMap =
  mapMaybeWithKey
  (\k v ->
    if elem k allPops then Just $ filter (flip elem allPops) v else Nothing
  ) nMap

mapRegionJson :: NeighbourMap -> NetworkJson -> Map RegionName Region
mapRegionJson nMap nwMap =
   Map.mapWithKey (\name pops -> {name : name, pops : mapPoPJson name nMap pops}) nwMap

mapPoPJson :: RegionName -> NeighbourMap -> PoPJson -> Map PoPName PoP
mapPoPJson regionName nMap popJson =
  Map.mapWithKey (\popName pop ->
                   let neighbours = Map.lookup popName nMap # fromMaybe nil
                   in
                    { name: popName
                    , geoLoc: pop.geoLoc
                    , servers: (\{ address
                                 , maxCpuCapacity
                                 , maxNetworkCapacity
                                 , receiveQueueCount
                                 , transmitQueueCount
                                 , capabilityTags
                                 , agents
                                 } ->
                                 Server {address
                                        , pop: popName
                                        , region: regionName
                                        , maxCpuCapacity
                                        , maxNetworkCapacity
                                        , receiveQueueCount
                                        , transmitQueueCount
                                        , capabilityTags
                                        , agents}
                               ) <$> pop.nodes
                    , neighbours}) popJson

processPoPJson :: ServerAddress -> (Map RegionName Region) -> NeighbourMap -> Either MultipleErrors (Tuple Server PoPInfo)
processPoPJson thisServerAddress regions nMap =
  let
    servers :: Map ServerAddress Server
    servers = foldl (\acc region ->
                      foldl (\innerAcc pop ->
                              foldl (\innerInnerAcc server@(Server {address}) ->
                                      Map.insert address server innerInnerAcc
                                    )
                              innerAcc
                              pop.servers
                            )
                      acc
                      region.pops
                    )
              Map.empty
              regions

    otherServers :: List Server
    otherServers = Map.lookup thisServerAddress servers
                   >>= (\server -> lookupPop regions (toServerLocation server))
                   <#> (\p -> p.servers)
                   <#> filter (\(Server {address}) -> address /= thisServerAddress)
                   # fromMaybe nil

    maybeThisServer :: Maybe Server
    maybeThisServer = Map.lookup thisServerAddress servers

    partitionPoPs :: PoPName -> Tuple (Maybe PoP) (List PoP)
    partitionPoPs thisPoP' =
      let
        updateAcc (Tuple mThisPop  otherPops) pop
          | pop.name == thisPoP' = Tuple (Just pop) otherPops
          | otherwise = Tuple mThisPop $ pop : otherPops
      in
        foldl (\acc region ->
                foldl updateAcc acc region.pops
              )
        (Tuple Nothing nil)
        regions

  in
   case maybeThisServer of
     Nothing -> Left $ NonEmptyList.singleton $ ForeignError "This node not present in any pop"
     Just thisServer@(Server {pop}) ->
       case partitionPoPs  pop of
         (Tuple (Just thisPoP') otherPoPs) ->
           Right (Tuple thisServer { regions: regions
                                   , servers: servers
                                   , otherServersInThisPoP: otherServers
                                   , otherPoPs: otherPoPs
                                   , thisLocation: toServerLocation thisServer
                                   , thisPoP: thisPoP'
                                   , neighbourMap: nMap
                                   })
         _ ->
           Left $ NonEmptyList.singleton $ ForeignError "This pop is not present in the neighbours map"

lookupPop :: Map RegionName Region -> ServerLocation -> Maybe PoP
lookupPop regions (ServerLocation {pop, region}) =
  (\{pops} -> Map.lookup pop pops) =<< Map.lookup region regions

finalise :: State -> Either MultipleErrors State -> Effect State
finalise state (Left errors) = do
  _ <- logWarning "Failed to process pop definition file" {misc: errors}
  pure state
finalise _ (Right state) =
  pure state

maybeLog :: forall a. String -> Maybe a -> Effect (Maybe a)
maybeLog _ val@(Just _) = pure $ val
maybeLog msg Nothing = do
  _ <- logWarning msg {}
  pure $ Nothing

toMap :: List PoP -> Map PoPName PoP
toMap list = foldl (\acc pop@{name} -> Map.insert name pop acc) Map.empty list


serverName :: ServerName State Msg
serverName = Names.popDefinitionName


--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domains :: List Atom
domains = serverName # Names.toDomain # singleton

logInfo :: forall a. Logger (Record a)
logInfo = domainLog Logger.info

logWarning :: forall a. Logger (Record a)
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: Record a} -> Logger (Record a)
domainLog = Logger.doLog domains
