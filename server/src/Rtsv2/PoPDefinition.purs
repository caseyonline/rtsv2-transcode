module Rtsv2.PoPDefinition
       ( startLink
       , getOtherServersForThisPoP
       , getOtherPoPs
       , getSeedsForThisPoP
       , getSeedsForOtherPoPs
       , thisNode
       , whereIsServer
       , thisLocation
       , Config
       , ServerAddress
       , ServerLocation(..)
       , PoPName
       , RegionName
       , PoP
       ) where

import Prelude

import Data.Either (Either(..), note')
import Data.Filterable (filter)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, catMaybes, index, length, nil, (:))
import Erl.Data.Map as Map
import File as File
import Foreign (Foreign, ForeignError(..), MultipleErrors)
import Logger as Logger
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub)
import Record as Record
import Rtsv2.Env as Env
import Shared.Utils (distinctRandomNumbers)
import Simple.JSON as JSON

type Config = { popDefinitionFile :: String
              }

type RegionName = String
type PoPName = String
type ServerAddress = String
data ServerLocation = ServerLocation PoPName RegionName

derive instance genericServerLocation :: Generic ServerLocation _
instance eqServerLocation :: Eq ServerLocation where
  eq = genericEq

type PoPInfo =
  { regions :: Map.Map RegionName Region
  , servers :: Map.Map ServerAddress ServerLocation
  , thisLocation :: ServerLocation
  , otherServersInThisPoP :: List ServerAddress
  , otherPoPs :: List PoP
  }

type State =  { config :: Config
              , regions :: Map.Map RegionName Region
              , servers :: Map.Map ServerAddress ServerLocation
              , thisNode :: ServerAddress
              , thisLocation :: ServerLocation
              , otherServersInThisPoP :: List ServerAddress
              , otherPoPs :: List PoP
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

thisNode :: Effect ServerAddress
thisNode = Gen.doCall serverName (\s -> pure $ CallReply s.thisNode s)

getOtherServersForThisPoP :: Effect (List ServerAddress)
getOtherServersForThisPoP = Gen.call serverName (\state@{otherServersInThisPoP} ->
                                                  CallReply otherServersInThisPoP state
                                                )

getOtherPoPs :: Effect (List PoP)
getOtherPoPs = Gen.call serverName (\state@{otherPoPs} ->
                                     CallReply otherPoPs state
                                   )

getSeedsForThisPoP :: Effect (List ServerAddress)
getSeedsForThisPoP = Gen.doCall serverName (\state@{otherServersInThisPoP} ->
                                             do
                                               seeds <- getSeedsForThisPoP' otherServersInThisPoP 4
                                               pure $ CallReply seeds state
                                           )

-- TODO - probably just want to give back all the otherServers
getSeedsForThisPoP' :: List ServerAddress -> Int -> Effect (List ServerAddress)
getSeedsForThisPoP' otherServersInThisPoP numRequired
  | otherServersInThisPoP == nil = pure $ nil
  | otherwise =
    do
      indexes <- distinctRandomNumbers (min numRequired (length otherServersInThisPoP))  ((length otherServersInThisPoP) - 1)
      let
        servers = map (\i -> index otherServersInThisPoP i) indexes
                  # sequence
                  # fromMaybe nil
      pure $ servers

getSeedsForOtherPoPs :: Effect (List PoP)
getSeedsForOtherPoPs = Gen.doCall serverName (\state@{otherPoPs} ->
                                             do
                                               seeds <- getSeedsForOtherPoPs' otherPoPs 4
                                               pure $ CallReply seeds state
                                           )

-- TODO - probably just want to give back all the otherServers
getSeedsForOtherPoPs' :: List PoP -> Int -> Effect (List PoP)
getSeedsForOtherPoPs' otherPoPs numRequired
  | otherPoPs == nil = pure $ nil
  | otherwise = catMaybes <$> (sequence $ foldl (\acc pop ->
                                                 getSeedsForOtherPoP' pop numRequired : acc
                                               )
                                               nil
                                               otherPoPs)

getSeedsForOtherPoP' :: PoP -> Int -> Effect (Maybe PoP)
getSeedsForOtherPoP' otherPoP@{servers} numRequired
  | servers == nil = pure Nothing
  | otherwise =
    do
      indexes <- distinctRandomNumbers (min numRequired (length servers))  ((length servers) - 1)
      let
        selectedServers = map (\i -> index servers i) indexes
                          # sequence
                          # fromMaybe nil
      pure $ Just otherPoP{servers = selectedServers}

thisLocation :: Effect ServerLocation
thisLocation = Gen.doCall serverName
             \state -> pure $ CallReply state.thisLocation state

whereIsServer :: ServerAddress -> Effect (Maybe ServerLocation)
whereIsServer sa = Gen.doCall serverName
             \state -> pure $ CallReply (Map.lookup sa state.servers) state

init :: Config -> Effect State
init config = do
  hostname <- Env.hostname
  ePopInfo <- readAndProcessPoPDefinition config hostname
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
        , otherServersInThisPoP : popInfo.otherServersInThisPoP
        , otherPoPs : popInfo.otherPoPs
        }

  _ <- logInfo "PoPDefinition Starting" {thisNode : hostname,
                                         otherServers : state.otherServersInThisPoP}
  _ <- Timer.sendAfter serverName 1000 Tick
  pure state


handleInfo :: Msg -> State -> Effect State
handleInfo msg state =
  case msg of
    Tick ->
      do
        ePopInfo <- readAndProcessPoPDefinition state.config state.thisNode
        newState <-  case ePopInfo of
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
        pure $ newState

readAndProcessPoPDefinition :: Config -> ServerAddress -> Effect (Either MultipleErrors PoPInfo)
readAndProcessPoPDefinition config hostName = do
  file <- readFile config.popDefinitionFile
  pure $ processPoPJson hostName =<< (mapRegionJson <$> (decodeJson =<< file))


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

processPoPJson :: ServerAddress -> (Map.Map RegionName Region) -> Either MultipleErrors PoPInfo
processPoPJson hostName regions =
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
    otherServers = Map.lookup hostName servers
                   >>= (\sl -> lookupPop regions sl)
                   <#> (\p -> p.servers)
                   <#> filter (\s -> s /= hostName)
                   # fromMaybe nil

    maybeThisPop = Map.lookup hostName servers

    otherPoPs :: PoPName -> List PoP
    otherPoPs thisPoP = let
                           addPoPToList list pop | pop.name == thisPoP = list
                                                 | otherwise = pop : list
                        in
                         foldl (\acc region ->
                                 foldl addPoPToList
                                 acc
                                 region.pops
                               )
                         nil
                         regions

  in
   case maybeThisPop of
     Nothing -> Left $ NonEmptyList.singleton $ ForeignError "This node not present in any pop"
     Just location@(ServerLocation popName _) ->
       Right { regions: regions
             , servers: servers
             , otherServersInThisPoP: otherServers
             , otherPoPs: otherPoPs popName
             , thisLocation: location
             }

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
