module Rtsv2.PoPDefinition
       ( startLink
       , getOtherServersForThisPoP
       , getOtherPoPs
       , thisNode
       , whereIsServer
       , thisLocation
       , thisPoP
       , PoP
       , Region
       ) where

import Prelude

import Data.Either (Either(..), note')
import Data.Filterable (filter)
import Data.Foldable (foldl)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, elem, nil, (:))
import Erl.Data.Map (Map, mapMaybeWithKey)
import Erl.Data.Map as Map
import File as File
import Foreign (Foreign, ForeignError(..), MultipleErrors)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub)
import Record as Record
import Rtsv2.Config (ServerLocation(..))
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Shared.Types (PoPName, RegionName, ServerAddress(..))
import Simple.JSON as JSON

type PoPInfo =
  { regions :: Map RegionName Region
  , servers :: Map ServerAddress ServerLocation
  , thisLocation :: ServerLocation
  , otherServersInThisPoP :: List ServerAddress
  , thisPoP :: PoP
  , otherPoPs :: List PoP
  }

type State =  { config :: Config.PoPDefinitionConfig
              , regions :: Map RegionName Region
              , servers :: Map ServerAddress ServerLocation
              , thisNode :: ServerAddress
              , thisLocation :: ServerLocation
              , thisPoP :: PoP
              , otherServersInThisPoP :: List ServerAddress
              , otherPoPs :: List PoP
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
type NeighbourMapJson =  Map PoPName (List PoPName)

data Msg = Tick

serverName :: ServerName State Msg
serverName = Local "PoPDefinition"

startLink :: Config.PoPDefinitionConfig -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

thisNode :: Effect ServerAddress
thisNode = Gen.doCall serverName (\s -> pure $ CallReply s.thisNode s)

thisPoP :: Effect PoP
thisPoP = Gen.doCall serverName (\s -> pure $ CallReply s.thisPoP s)

getOtherServersForThisPoP :: Effect (List ServerAddress)
getOtherServersForThisPoP = Gen.call serverName (\state@{otherServersInThisPoP} ->
                                                  CallReply otherServersInThisPoP state
                                                )

getOtherPoPs :: Effect (List PoP)
getOtherPoPs = Gen.doCall serverName
  \state@{otherPoPs} -> pure $ CallReply otherPoPs state

thisLocation :: Effect ServerLocation
thisLocation = Gen.doCall serverName
             \state -> pure $ CallReply state.thisLocation state

whereIsServer :: ServerAddress -> Effect (Maybe ServerLocation)
whereIsServer sa = Gen.doCall serverName
             \state -> pure $ CallReply (Map.lookup sa state.servers) state

init :: Config.PoPDefinitionConfig -> Effect State
init config = do
  hostname <- ServerAddress <$> Env.hostname
  eNeighbourMap <- readNeighbourMap config
  neighbourMap <-  case eNeighbourMap of
    Left e -> do
               _ <- Logger.warning "Failed to process WAN definition file" {misc: e}
               unsafeCrashWith "invalid WAN definition file"
    Right r -> pure r

  ePopInfo <- readAndProcessPoPDefinition config hostname neighbourMap
  popInfo <-  case ePopInfo of
    Left e -> do
               _ <- Logger.warning "Failed to process pop definition file" {misc: e}
               unsafeCrashWith "invalid pop definition file"
    Right r -> pure r
  let
      state =
        { config : config
        , regions : popInfo.regions
        , servers : popInfo.servers
        , thisNode : hostname
        , thisLocation : popInfo.thisLocation
        , thisPoP : popInfo.thisPoP
        , otherPoPs : popInfo.otherPoPs
        , otherServersInThisPoP : popInfo.otherServersInThisPoP
        }

  _ <- logInfo "PoPDefinition Starting" {thisNode : hostname,
                                         otherServers : state.otherServersInThisPoP}
  _ <- Timer.sendAfter serverName 1000 Tick
  pure state

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state =
  case msg of
    Tick ->
      do
        eNeighbourMap <- readNeighbourMap state.config
        newState <-  case eNeighbourMap of
          Left e -> do _ <- Logger.warning "Failed to process WAN definition file" {misc: e}
                       pure state
          Right neighbourMap -> do
            ePopInfo <- readAndProcessPoPDefinition state.config state.thisNode neighbourMap
            case ePopInfo of

              Left e -> do _ <- Logger.warning "Failed to process pop definition file" {misc: e}
                           pure state
              Right popInfo@{thisLocation : newPoP}
                | newPoP == state.thisLocation  ->
                  pure $ state { regions = popInfo.regions
                               , servers = popInfo.servers
                               , otherServersInThisPoP = popInfo.otherServersInThisPoP
                               , otherPoPs = popInfo.otherPoPs
                               }
                | otherwise ->
                    do _ <- Logger.warning "This node seems to have changed pop - ignoring" { currentPoP: state.thisLocation
                                                                                            , filePoP : newPoP
                                                                                            }
                       pure state

        _ <- Timer.sendAfter serverName 1000 Tick
        pure $ CastNoReply newState

readNeighbourMap :: Config.PoPDefinitionConfig -> Effect (Either MultipleErrors NeighbourMapJson)
readNeighbourMap config = do
  file <- readFile $ joinWith "/" [config.directory, config.wanDefinitionFile]
  pure $ JSON.readJSON =<< file

readAndProcessPoPDefinition :: Config.PoPDefinitionConfig -> ServerAddress -> NeighbourMapJson -> Effect (Either MultipleErrors PoPInfo)
readAndProcessPoPDefinition config hostName neighbourMap = do
  -- In Effect
  file <- readFile $ joinWith "/" [config.directory, config.popDefinitionFile]
  let ePoPJson =(JSON.readJSON =<< file)
  pure do -- In either
        popJson <- ePoPJson
        let allPops = Map.keys =<< Map.values popJson
            filteredNeighbours = filterNeighbours allPops neighbourMap
            regionMap = mapRegionJson filteredNeighbours popJson
        processPoPJson hostName regionMap


readFile :: String -> Effect (Either MultipleErrors String)
readFile fileName = do
  jsonString <- File.readUtf8File fileName
  pure $ note' (\_ -> NonEmptyList.singleton $ ForeignError ("failed to read file " <> fileName)) jsonString

filterNeighbours :: List PoPName -> NeighbourMapJson -> NeighbourMapJson
filterNeighbours allPops neighbourMap =
  mapMaybeWithKey
  (\k v ->
    if elem k allPops then Just $ filter (flip elem allPops) v else Nothing
  ) neighbourMap

mapRegionJson :: NeighbourMapJson -> NetworkJson -> Map RegionName Region
mapRegionJson neighbourMap nwMap =
   Map.mapWithKey (\name pops -> {name : name, pops : mapPoPJson neighbourMap pops}) nwMap

mapPoPJson :: NeighbourMapJson -> PoPJson -> Map PoPName PoP
mapPoPJson neighbourMap =
  Map.mapWithKey (\name servers ->
                   let neighbours = Map.lookup name neighbourMap # fromMaybe nil
                   in
                    {name : name, servers : servers, neighbours})

processPoPJson :: ServerAddress -> (Map RegionName Region) -> Either MultipleErrors PoPInfo
processPoPJson hostName regions =
  let
    servers :: Map ServerAddress ServerLocation
    servers = foldl (\acc region ->
                      foldl (\innerAcc pop ->
                              foldl (\innerInnerAcc server ->
                                      Map.insert server (ServerLocation pop.name region.name) innerInnerAcc
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
    otherServers = Map.lookup hostName servers
                   >>= (\sl -> lookupPop regions sl)
                   <#> (\p -> p.servers)
                   <#> filter (\s -> s /= hostName)
                   # fromMaybe nil

    maybeThisLocation = Map.lookup hostName servers

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
     Just location@(ServerLocation popName _) ->
       case partitionPoPs popName of
         (Tuple (Just thisPoP') otherPoPs) ->
            Right { regions: regions
                  , servers: servers
                  , otherServersInThisPoP: otherServers
                  , otherPoPs: otherPoPs
                  , thisLocation: location
                  , thisPoP: thisPoP'
                  }
         _ ->
           Left $ NonEmptyList.singleton $ ForeignError "This pop is not present in the neighbours map"

lookupPop :: Map RegionName Region -> ServerLocation -> Maybe PoP
lookupPop regions (ServerLocation pop region) =
  (\{pops} -> Map.lookup pop pops) =<< Map.lookup region regions

finalise :: State -> Either MultipleErrors State -> Effect State
finalise state (Left errors) = do
  _ <- Logger.warning "Failed to process pop definition file" {misc: errors}
  pure state
finalise _ (Right state) =
  pure state

maybeLog :: forall a. String -> Maybe a -> Effect (Maybe a)
maybeLog _ val@(Just _) = pure $ val
maybeLog msg Nothing = do
  _ <- Logger.warning msg {}
  pure $ Nothing

logInfo :: forall a b. Nub (domain :: List Atom | a) b =>  String -> Record a -> Effect Foreign
logInfo msg metaData =
  Logger.info msg (Record.merge {domain : ((atom "PopDefinition"): nil)} {misc: metaData})
