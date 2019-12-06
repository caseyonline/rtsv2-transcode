module Rtsv2.TransPoPAgent where

import Prelude

import Bus as Bus
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process (spawnLink)
import Erl.Utils (sleep, systemTime, TimeUnit(..))
import Foreign (Foreign)
import Logger as Logger
import Os (osCmd)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub)
import Record as Record
import Rtsv2.Env as Env
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.IntraPoPSerf as IntraSerf
import Rtsv2.PoPDefinition (PoP, ServerAddress, ServerLocation(..), PoPName, whereIsServer)
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.TransPoPSerf (IpAndPort, origin)
import Rtsv2.TransPoPSerf as TransSerf
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import SpudGun as SpudGun

type Config
  = { bindPort :: Int
    , rpcPort :: Int
    , leaderTimeoutMs :: Int
    , leaderAnnounceMs :: Int
    , rejoinEveryMs :: Int
    }

serverName :: ServerName State Msg
serverName = Local "transPopAgent"

data Msg
  = Tick
  | JoinAll
  | IntraPoPMsg IntraSerf.IntraMessage
  | TransPoPMsg TransSerf.TransMessage

startLink :: Config -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

type State
  = { currentLeader :: Maybe ServerAddress
    , weAreLeader :: Boolean
    , lastLeaderAnnouncement :: Int
    , leaderAnnouncementTimeout :: Int
    , thisNode :: ServerAddress
    , thisLocation :: ServerLocation
    , config :: Config
    , serfRpcAddress :: IpAndPort
    , members :: Map PoPName (Set.Set String)
    }

init :: Config -> Effect State
init config@{ leaderTimeoutMs
            , leaderAnnounceMs
            , rejoinEveryMs
            , rpcPort
            } = do
  _ <- logInfo "Trans-PoP Agent Starting" {config: config}
  _ <- Gen.registerExternalMapping serverName (\m -> TransPoPMsg <$> (TransSerf.messageMapper m))
  _ <- Bus.subscribe serverName IntraPoPAgent.bus IntraPoPMsg
  _ <- Timer.sendEvery serverName leaderAnnounceMs Tick
  _ <- Timer.sendAfter serverName (min (floor ((toNumber leaderAnnounceMs) * 1.5)) rejoinEveryMs) JoinAll
  _ <- Timer.sendEvery serverName rejoinEveryMs JoinAll
  rpcBindIp <- Env.privateInterfaceIp
  thisNode <- PoPDefinition.thisNode
  thisLocation <- PoPDefinition.thisLocation
  now <- systemTime MilliSecond
  let
    serfRpcAddress =
      { ip: show rpcBindIp
      , port: rpcPort
      }

  pure
    $ { config: config
      , currentLeader: Nothing
      , weAreLeader: false
      , lastLeaderAnnouncement: now
      , leaderAnnouncementTimeout: leaderTimeoutMs
      , thisNode: thisNode
      , thisLocation: thisLocation
      , serfRpcAddress: serfRpcAddress
      , members : Map.empty
      }

handleInfo :: Msg -> State -> Effect State
handleInfo msg state@{ thisNode } = case msg of
  Tick -> handleTick state
  JoinAll -> joinAllSerf state
  IntraPoPMsg intraMessage -> handleIntraPoPMessage intraMessage state
  TransPoPMsg transMessage
    | origin transMessage == Nothing -> pure state
    | origin transMessage == Just thisNode -> pure state
    | otherwise -> handleTransPoPMessage transMessage state

handleIntraPoPMessage :: IntraSerf.IntraMessage -> State -> Effect State
handleIntraPoPMessage IntraSerf.Ignore state = pure state

handleIntraPoPMessage (IntraSerf.TransPoPLeader address) state = handleLeaderAnnouncement address state

handleIntraPoPMessage (IntraSerf.StreamAvailable streamId addr) state = handleIntraPoPStreamAvailable streamId addr state

handleIntraPoPMessage (IntraSerf.MembersAlive members) state = pure state

handleIntraPoPMessage (IntraSerf.MembersLeft members) state = pure state

handleTransPoPMessage :: TransSerf.TransMessage -> State -> Effect State
handleTransPoPMessage TransSerf.Ignore state = pure state

handleTransPoPMessage TransSerf.SerfStreamFailed state = pure state

handleTransPoPMessage (TransSerf.StreamAvailable streamId server) state = do
  _ <- IntraPoPAgent.announceRemoteStreamIsAvailable streamId server
  pure state

handleTransPoPMessage (TransSerf.MembersAlive aliveMembers) state =
  do
    _ <- logInfo "Members Alive" {members: aliveMembers}
    newMembers <- foldl addPoP (pure state.members) aliveMembers
    pure state {members = newMembers}
  where
    addPoP :: Effect (Map PoPName (Set.Set String)) -> TransSerf.SerfMember -> Effect (Map PoPName (Set.Set String))
    addPoP eMembers aliveMember@{name: aliveName} =
      do
        members <- eMembers
        mPoP <- getPoP aliveMember
        pure $ fromMaybe members ((\popName ->
                                    let
                                      existingMembers = case Map.lookup popName members of
                                                          Just existing -> existing
                                                          Nothing -> Set.empty
                                      newMembers = Set.insert aliveName existingMembers
                                    in
                                     Map.insert popName newMembers members
                                  ) <$> mPoP)

handleTransPoPMessage (TransSerf.MembersLeft leftMembers) state =
  do
    _ <- logInfo "Members Left" {members: leftMembers}
    newMembers <- foldl removePoP (pure state.members) leftMembers
    pure state {members = newMembers}
  where
    removePoP :: Effect (Map PoPName (Set.Set String)) -> TransSerf.SerfMember -> Effect (Map PoPName (Set.Set String))
    removePoP eMembers leftMember@{name: leftName} =
      do
        members <- eMembers
        mPoP <- getPoP leftMember
        pure $ fromMaybe members ((\popName ->
                                    let
                                      existingMembers = case Map.lookup popName members of
                                                          Just existing -> existing
                                                          Nothing -> Set.empty
                                      newMembers = Set.delete leftName existingMembers
                                    in
                                     Map.insert popName newMembers members
                                  ) <$> mPoP)

getPoP :: TransSerf.SerfMember -> Effect (Maybe PoPName)
getPoP {name : memberName} =
  do
    mServerLocation <- whereIsServer memberName
    pure $ case mServerLocation of
      Nothing -> Nothing
      Just (ServerLocation popName _) -> Just popName

--TransPopMsg a ->
-- TODO - if we are leader and transpop-streamAvailable from other PoP, then call IntraPop.streamIsAvailable
handleIntraPoPStreamAvailable :: StreamId -> ServerAddress -> State -> Effect State
handleIntraPoPStreamAvailable _ _ state@{ weAreLeader: false } = pure state

handleIntraPoPStreamAvailable streamId addr state@{ thisLocation: (ServerLocation pop _)
                                                  , serfRpcAddress
                                                  } = do
  mServerLocation <- whereIsServer addr
  case mServerLocation of
    Nothing -> pure state
    Just (ServerLocation sourcePoP _)
      | sourcePoP == pop -> do
        -- Message from our pop - distribute over trans-pop
        _ <- logInfo "Local stream being delivered to trans-pop" { streamId: streamId }
        result <- TransSerf.event state.serfRpcAddress "streamAvailable" (TransSerf.StreamAvailable streamId addr) true
        _ <- logInfo "trans-pop serf said" { result: result }
        pure state
      | otherwise -> pure state

handleTick :: State -> Effect State
handleTick state@{ weAreLeader: true } = do
  _ <- IntraPoPAgent.announceTransPoPLeader
  pure state

handleTick state@{ lastLeaderAnnouncement
                 , leaderAnnouncementTimeout
                 , serfRpcAddress
                 } = do
  now <- systemTime MilliSecond
  if lastLeaderAnnouncement + leaderAnnouncementTimeout < now then do
    _ <- logInfo "Leader is absent, becoming leader" {}
    startResp <- osCmd "scripts/startTransPoPAgent.sh"
    _ <- logInfo "Start Resp" {resp: startResp}
    _ <- IntraPoPAgent.announceTransPoPLeader
    x <- loopStreamJoin serfRpcAddress 5
    _ <- logInfo "Stream Resp" {resp: x}
    pure
      $ state
          { lastLeaderAnnouncement = now
          , weAreLeader = true
          }
  else do
    pure $ state

  where
    loopStreamJoin rpcAddress n =
      do
      _ <- sleep 300
      resp <- TransSerf.stream rpcAddress
      case resp of
          Left _ ->
            case n of
                 0 -> pure resp
                 _ -> loopStreamJoin serfRpcAddress $ n - 1
          Right x ->
            do
            _ <- logInfo "Joined Trans-PoP stream" {}
            pure resp


handleLeaderAnnouncement :: ServerAddress -> State -> Effect State
handleLeaderAnnouncement address state@{ thisNode
                                       , weAreLeader: true
                                       , serfRpcAddress
                                       }
  | address < thisNode = do
    result <- TransSerf.leave serfRpcAddress
    _ <- logInfo "Another node has taken over as transpop leader; stepping down" { leader: address }
    stopResp <- osCmd "scripts/stopTransPoPAgent.sh"
    _ <- logInfo "Stop Resp" {resp: stopResp}

    now <- systemTime MilliSecond
    pure
      $ state
          { currentLeader = Just address
          , lastLeaderAnnouncement = now
          , weAreLeader = false
          , members = Map.empty :: Map PoPName (Set.Set String)
          }

handleLeaderAnnouncement address state@{ weAreLeader: true } =
  pure state

handleLeaderAnnouncement address state@{ currentLeader
                                       , thisNode
                                       }
  | Just address /= currentLeader
      && address /= thisNode = do
    _ <- logInfo "Another node has announced as transpop leader, remember which one" { leader: address }
    now <- systemTime MilliSecond
    pure
      $ state
          { currentLeader = Just address
          , lastLeaderAnnouncement = now
          }

handleLeaderAnnouncement address state = do
  now <- systemTime MilliSecond
  pure $ state { lastLeaderAnnouncement = now }

joinAllSerf :: State -> Effect State
joinAllSerf state@{ weAreLeader: false } = do
  pure state

joinAllSerf state@{ config, serfRpcAddress } =
  let
    serverAddressToSerfAddress :: String -> IpAndPort
    serverAddressToSerfAddress s =
      { ip: s
      , port: config.bindPort
      }
  in
    do
      pops <- PoPDefinition.getSeedsForOtherPoPs :: Effect (List PoP)
      _ <-
        sequence
          $ foldl
              ( \acc { name, servers } ->
                  ( spawnLink
                      ( \_ -> do
                          foldl
                            ( \iAcc server -> do
                                restResult <- SpudGun.getText ("http://" <> server <> ":3000/poc/api/transPoPLeader")
                                _ <- case restResult of
                                  Nothing -> pure unit
                                  Just addr -> do
                                    result <- TransSerf.join serfRpcAddress ((serverAddressToSerfAddress addr) : nil) true
                                    _ <-
                                      logInfo "Join said "
                                        { server: addr
                                        , restResult: restResult
                                        , serfResult: result
                                        }
                                    pure unit
                                pure unit
                            )
                            mempty
                            servers
                      )
                  )
                    : acc
              )
              nil
              pops
      pure state

logInfo :: forall a b. Nub ( domain :: List Atom | a ) b => String -> Record a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.TransPoP)) : nil) } { misc: metaData })
