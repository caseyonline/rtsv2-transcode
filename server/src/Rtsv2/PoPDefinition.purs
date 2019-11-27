module Rtsv2.PoPDefinition
       (startLink
       , init
       , serverName
       , Config
       ) where

import Prelude

import Data.Either (Either(..), note')
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Data.List (List(..), nil, singleton, (:))
import Erl.Data.Map (Map, empty, fromFoldable, insert, mapWithKey)
import File as File
import Foreign (ForeignError(..), MultipleErrors)
import Logger as Logger
import Os (getEnv)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CastResult(..))
import Pinto.Gen as Gen
import Pinto.Gen as PintoGen
import Pinto.Timer as Timer
import Shared.Utils (lazyCrashIfMissing)
import Simple.JSON as JSON

type Config = { popDefinitionFile :: String
              }

type RegionName = String
type PoPName = String
type ServerAddress = String
data ServerLocation = ServerLocation PoPName RegionName

type State =  { config :: Config
              , regions :: Map RegionName Region
              , servers :: Map ServerAddress ServerLocation
              , thisNode :: ServerAddress
              }

type Region = { name :: RegionName
              , pops :: Map PoPName PoP
              }

type PoP = { name :: PoPName
           , servers :: List ServerAddress
           }

type PoPJsonFormat = Map String (Map String (List String))

serverName :: ServerName State
serverName = ServerName "popDefinition"

startLink :: Config -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

init :: Config -> Effect State
init config = do
  maybeHostName <- getEnv "HOST"
  let
    hostName = fromMaybe' (lazyCrashIfMissing "No Hostname available") maybeHostName
  newState <- readAndProcessPoPDefinition { config : config
                                          , regions : empty
                                          , servers : empty
                                          , thisNode : hostName }
  _ <- Timer.sendAfter 1000 handleTick
  pure $ newState

handleTick :: Effect Unit
handleTick = PintoGen.doCast serverName \state -> do
  newState <- readAndProcessPoPDefinition state
  _ <- Timer.sendAfter 1000 handleTick
  pure $ CastNoReply newState

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

mapRegionJson :: PoPJsonFormat -> Map RegionName Region
mapRegionJson =
  mapWithKey (\name pops -> {name : name, pops : mapPoPJson pops})

mapPoPJson :: Map String (List String) -> Map PoPName PoP
mapPoPJson =
  mapWithKey (\name servers -> {name : name, servers : servers})

updateState :: State -> Map RegionName Region -> State
updateState state regions =
  let
    servers :: Map ServerAddress ServerLocation
    servers = foldl (\acc region ->
                      foldl (\innerAcc pop ->
                              foldl (\innerInnerAcc server ->
                                      insert server (ServerLocation pop.name region.name) innerInnerAcc
                                    )
                              innerAcc
                              pop.servers
                            )
                      acc
                      region.pops
                    )
              empty
              regions
  in
   state {regions = regions,
          servers = servers}

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
