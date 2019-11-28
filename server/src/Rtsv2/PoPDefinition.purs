module Rtsv2.PoPDefinition
       (startLink
       , getSeeds
       , Config
       ) where

import Prelude

import Control.Apply (lift2)
import Data.Either (Either(..), note')
import Data.Filterable (filter)
import Data.Foldable (foldl)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Set as Set
import Data.Traversable (sequence)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Random (randomInt)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, fromFoldable, index, length, nil, (:))
import Erl.Data.Map as Map
import File as File
import Foreign (Foreign, ForeignError(..), MultipleErrors)
import Logger as Logger
import Os (getEnv)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub)
import Record as Record
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as JSON

type Config = { popDefinitionFile :: String
              }

type RegionName = String
type PoPName = String
type ServerAddress = String
data ServerLocation = ServerLocation PoPName RegionName

type State =  { config :: Config
              , regions :: Map.Map RegionName Region
              , servers :: Map.Map ServerAddress ServerLocation
              , thisNode :: ServerAddress
              , otherServersInThisPoP :: List ServerAddress
              }

type Region = { name :: RegionName
              , pops :: Map.Map PoPName PoP
              }

type PoP = { name :: PoPName
           , servers :: List ServerAddress
           }

type PoPJsonFormat = Map.Map String (Map.Map String (List String))

data Msg = Tick

serverName :: ServerName State Msg
serverName = Local "popDefinition"

startLink :: Config -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName (init args) handleInfo

getSeeds :: Effect (List ServerAddress)
getSeeds = Gen.doCall serverName (\s -> getSeeds' s >>= (\r -> pure $ CallReply r s))

-- TODO - this current picks 4 random seeds - probably just want to give back all the otherServers
--        If we do stick with random, then we need to ensure required count <= length(otherServers)
getSeeds' :: State -> Effect (List ServerAddress)
getSeeds' state@{otherServersInThisPoP}
  | otherServersInThisPoP == nil = pure $ nil
  | otherwise =
    do
      indexes <- randomNumbers 4 ((length otherServersInThisPoP) - 1)
      let
        servers = map (\i -> index otherServersInThisPoP i) indexes
                  # sequence
                  # fromMaybe nil
      pure $ servers

init :: Config -> Effect State
init config = do
  maybeHostName <- getEnv "HOSTNAME"
  let
    hostName = fromMaybe' (lazyCrashIfMissing "No Hostname available") maybeHostName
  newState <- readAndProcessPoPDefinition { config : config
                                          , regions : Map.empty
                                          , servers : Map.empty
                                          , thisNode : hostName
                                          , otherServersInThisPoP : nil
                                          }
  _ <- logInfo "PoPDefinition Starting" {thisNode : hostName,
                                         otherServers : newState.otherServersInThisPoP}
  _ <- Timer.sendAfter serverName 1000 Tick
  pure $ newState

handleInfo :: Msg -> State -> Effect State
handleInfo msg state =
  case msg of
    Tick ->
      do
        newState <- readAndProcessPoPDefinition state
        _ <- Timer.sendAfter serverName 1000 Tick
        pure $ newState

randomNumbers :: Int -> Int -> Effect (List Int)
randomNumbers n max =
  randomNumbers_ n max (pure Set.empty)
  <#> fromFoldable

randomNumbers_ :: Int -> Int -> Effect (Set.Set Int) -> Effect (Set.Set Int)
randomNumbers_ n max set =
  do
    set2 <- addDistinct max set
    if Set.size set2 == n then
      pure $ set2
      else
      randomNumbers_ n max (pure set2)

addDistinct :: Int -> Effect (Set.Set Int) -> Effect (Set.Set Int)
addDistinct max set =
  lift2 Set.insert (randomInt 0 max) set

readAndProcessPoPDefinition :: State -> Effect State
readAndProcessPoPDefinition state = do
  file <- readFile state.config.popDefinitionFile
  let
    newState = updateState state <$> mapRegionJson <$> (decodeJson =<< file)
  finalise state newState

readFile :: String -> Effect (Either MultipleErrors String)
readFile fileName = do
  jsonString <- File.readUtf8File fileName
  pure $ note' (\_ -> NonEmptyList.singleton $ ForeignError ("failed to read file " <> fileName)) jsonString

decodeJson :: String -> Either MultipleErrors PoPJsonFormat
decodeJson = JSON.readJSON

mapRegionJson :: PoPJsonFormat -> Map.Map RegionName Region
mapRegionJson =
  Map.mapWithKey (\name pops -> {name : name, pops : mapPoPJson pops})

mapPoPJson :: Map.Map String (List String) -> Map.Map PoPName PoP
mapPoPJson =
  Map.mapWithKey (\name servers -> {name : name, servers : servers})

updateState :: State -> Map.Map RegionName Region -> State
updateState state@{thisNode} regions =
  let
    servers :: Map.Map ServerAddress ServerLocation
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

    otherServers :: List String
    otherServers = Map.lookup thisNode servers
                   >>= (\sl -> lookupPop regions sl)
                   <#> (\p -> p.servers)
                   <#> filter (\s -> s /= thisNode)
                   # fromMaybe nil

  in
   state { regions = regions
         , servers = servers
         , otherServersInThisPoP = otherServers }

lookupPop :: Map.Map RegionName Region -> ServerLocation -> Maybe PoP
lookupPop regions (ServerLocation pop region) =
  (\{pops : pops} -> Map.lookup pop pops) =<< Map.lookup region regions

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
