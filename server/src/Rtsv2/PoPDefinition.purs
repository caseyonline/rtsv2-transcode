module Rtsv2.PoPDefinition
       ( startLink
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
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Erl.Atom (Atom)
import Erl.Data.List (List, index, length, nil, singleton, (:))
import Erl.Data.Map (Map, mapMaybeWithKey)
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
import Shared.Types (PoPName, RegionName, Server(..), ServerAddress(..), ServerLocation(..), extractPoP, toServer)
import Simple.JSON as JSON

type PoPInfo =
  { regions :: Map RegionName Region
  , servers :: Map ServerAddress ServerLocation
  , thisLocation :: ServerLocation
  , otherServersInThisPoP :: List ServerAddress
  , thisPoP :: PoP
  , otherPoPs :: List PoP
  , neighbourMap :: NeighbourMap
  }

type State =  { config :: Config.PoPDefinitionConfig
              , regions :: Map RegionName Region
              , servers :: Map ServerAddress ServerLocation
              , thisServer :: Server
              , otherServersInThisPoP :: List ServerAddress
              , otherPoPs :: Map PoPName PoP
              , neighbourMap :: NeighbourMap
              }

type Region = { name :: RegionName
              , pops :: Map PoPName PoP
              }

type PoP = { name :: PoPName
           , servers :: List ServerAddress
           , neighbours :: List PoPName
           }

type NetworkJson = Map RegionName PoPJson
type PoPJson =  Map PoPName (List ServerAddress)
type NeighbourMap =  Map PoPName (List PoPName)

data Msg = Tick

startLink :: Config.PoPDefinitionConfig -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

neighbourMap :: Effect NeighbourMap
neighbourMap = exposeState _.neighbourMap serverName

getOtherServersForThisPoP :: Effect (List ServerAddress)
getOtherServersForThisPoP = exposeState _.otherServersInThisPoP serverName

getOtherPoPs :: Effect (Map PoPName PoP)
getOtherPoPs = exposeState _.otherPoPs serverName

getOtherPoPNames :: Effect (List PoPName)
getOtherPoPNames = exposeState (Map.keys <<< _.otherPoPs) serverName

getThisServer :: Effect Server
getThisServer = exposeState _.thisServer serverName

getRandomServerInPoP :: PoPName -> Effect (Maybe ServerAddress)
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


whereIsServer :: ServerAddress -> Effect (Maybe ServerLocation)
whereIsServer sa = Gen.doCall serverName
             \state -> pure $ CallReply (Map.lookup sa state.servers) state

serversInThisPoPByAddress :: Effect (Map ServerAddress Server)
serversInThisPoPByAddress =
  -- TODO - this probasbly need to list which services each node offers as well
  Gen.doCall serverName
    \state@{thisServer} ->
      let
        thisPoP = extractPoP thisServer
        result
          = state.servers
            # Map.mapMaybeWithKey (\address location ->
                                    if extractPoP location == thisPoP then
                                      Just $ (toServer address  location)
                                    else
                                      Nothing
                                  )
      in
      pure $ CallReply result state


init :: Config.PoPDefinitionConfig -> Effect State
init config = do
  thisServerAddress <- ServerAddress <$> Env.hostname
  eNeighbourMap <- readNeighbourMap config
  nMap <-  case eNeighbourMap of
    Left e -> do
               _ <- logWarning "Failed to process WAN definition file" {misc: e}
               unsafeCrashWith "invalid WAN definition file"
    Right r -> pure r

  ePopInfo <- readAndProcessPoPDefinition config thisServerAddress nMap
  popInfo <-  case ePopInfo of
    Left e -> do
               _ <- logWarning "Failed to process pop definition file" {misc: e}
               unsafeCrashWith "invalid pop definition file"
    Right r -> pure r
  let
      thisServer' =  toServer thisServerAddress popInfo.thisLocation
      state =
        { config : config
        , regions : popInfo.regions
        , servers : popInfo.servers
        , thisServer : thisServer'
        , otherPoPs : toMap popInfo.otherPoPs
        , otherServersInThisPoP : popInfo.otherServersInThisPoP
        , neighbourMap : popInfo.neighbourMap
        }

  _ <- logInfo "PoPDefinition Starting" { thisServer : thisServer'
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
              Right popInfo@{thisLocation : (ServerLocation newPoPLocation)}
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

readAndProcessPoPDefinition :: Config.PoPDefinitionConfig -> ServerAddress -> NeighbourMap -> Effect (Either MultipleErrors PoPInfo)
readAndProcessPoPDefinition config thisServerAddress nMap = do
  -- In Effect
  file <- readFile $ joinWith "/" [config.directory, config.popDefinitionFile]
  let ePoPJson =(JSON.readJSON =<< file)
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
   Map.mapWithKey (\name pops -> {name : name, pops : mapPoPJson nMap pops}) nwMap

mapPoPJson :: NeighbourMap -> PoPJson -> Map PoPName PoP
mapPoPJson nMap =
  Map.mapWithKey (\name servers ->
                   let neighbours = Map.lookup name nMap # fromMaybe nil
                   in
                    {name : name, servers : servers, neighbours})

processPoPJson :: ServerAddress -> (Map RegionName Region) -> NeighbourMap -> Either MultipleErrors PoPInfo
processPoPJson thisServerAddress regions nMap =
  let
    servers :: Map ServerAddress ServerLocation
    servers = foldl (\acc region ->
                      foldl (\innerAcc pop ->
                              foldl (\innerInnerAcc server ->
                                      Map.insert server (ServerLocation {pop: pop.name, region:  region.name}) innerInnerAcc
                                    )
                              innerAcc
                              pop.servers
                            )
                      acc
                      region.pops
                    )
              Map.empty
              regions

    otherServers :: List ServerAddress
    otherServers = Map.lookup thisServerAddress servers
                   >>= (\sl -> lookupPop regions sl)
                   <#> (\p -> p.servers)
                   <#> filter (\s -> s /= thisServerAddress)
                   # fromMaybe nil

    maybeThisLocation = Map.lookup thisServerAddress servers

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
   case maybeThisLocation of
     Nothing -> Left $ NonEmptyList.singleton $ ForeignError "This node not present in any pop"
     Just sl@(ServerLocation location) ->
       case partitionPoPs location.pop of
         (Tuple (Just thisPoP') otherPoPs) ->
            Right { regions: regions
                  , servers: servers
                  , otherServersInThisPoP: otherServers
                  , otherPoPs: otherPoPs
                  , thisLocation: sl
                  , thisPoP: thisPoP'
                  , neighbourMap: nMap
                  }
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

logInfo :: forall a. Logger a
logInfo = domainLog Logger.info

logWarning :: forall a. Logger a
logWarning = domainLog Logger.warning

domainLog :: forall a. Logger {domain :: List Atom, misc :: a} -> Logger a
domainLog = Logger.doLog domains
