module Rtsv2.TransPoPAgent
       ( announceStreamIsAvailable
       , announceStreamStopped
       , announceTransPoPLeader
       , health
       , startLink
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Set as Set
import Data.Traversable (sequence, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Ephemeral.Map (EMap)
import Ephemeral.Map as EMap
import Erl.Atom (atom)
import Erl.Data.List (List, index, length, nil, zip, (:))
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process (spawnLink)
import Erl.Utils (Milliseconds, sleep, systemTimeMs)
import Foreign (Foreign)
import Logger as Logger
import Math (sqrt)
import Os (osCmd)
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub, class Union)
import Record as Record
import Rtsv2.Config (ServerLocation(..))
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Health (Health)
import Rtsv2.Health as Health
import Rtsv2.PoPDefinition (PoP, whereIsServer)
import Rtsv2.PoPDefinition as PoPDefinition
import Serf (IpAndPort)
import Serf as Serf
import Shared.Agent as Agent
import Shared.Stream (StreamId)
import Shared.Types (PoPName, ServerAddress(..))
import Shared.Utils (distinctRandomNumbers)
import SpudGun as SpudGun

type State
  = { intraPoPApi :: Config.IntraPoPAgentApi
    , currentLeader :: Maybe ServerAddress
    , weAreLeader :: Boolean
    , lastLeaderAnnouncement :: Milliseconds
    , leaderAnnouncementTimeout :: Milliseconds
    , thisNode :: ServerAddress
    , thisLocation :: ServerLocation
    , thisPoP :: PoPName
    , config :: Config.TransPoPAgentConfig
    , serfRpcAddress :: IpAndPort
    , members :: Map PoPName (Set.Set ServerAddress)
    , streamStateClocks :: EMap StreamId Int
    }

data StreamState = StreamAvailable
                 | StreamStopped

data TransMessage = StreamState StreamState StreamId ServerAddress

data Msg
  = Tick
  | JoinAll
  | ConnectStream
  | TransPoPSerfMsg (Serf.SerfMessage TransMessage)

serverName :: ServerName State Msg
serverName = Local $ show Agent.TransPoP

messageOrigin :: TransMessage -> Effect (Maybe PoPName)
messageOrigin (StreamState _ _ addr) = originPoP addr

originPoP :: ServerAddress -> Effect (Maybe PoPName)
originPoP addr =
  do
    mServerLocation <- whereIsServer addr
    pure $ case mServerLocation of
      Nothing -> Nothing
      Just (ServerLocation sourcePoP _) -> Just sourcePoP

health :: Effect Health
health =
  Gen.doCall serverName \state -> do
    currentHealth <- getHealth state
    pure $ CallReply currentHealth state
  where
    getHealth {weAreLeader: false} = pure Health.NA
    getHealth {members} = do
      allOtherPoPs <- PoPDefinition.getOtherPoPs
      pure $ Health.percentageToHealth $ (Map.size members * 100) / (length allOtherPoPs) * 100

announceStreamIsAvailable :: StreamId -> ServerAddress -> Effect Unit
announceStreamIsAvailable streamId addr =
  Gen.doCast serverName ((map CastNoReply) <<< doAnnounceStreamIsAvailable)
  where
    doAnnounceStreamIsAvailable :: State -> Effect State
    doAnnounceStreamIsAvailable state@{ weAreLeader: false } = pure state

    doAnnounceStreamIsAvailable state@{ thisLocation: (ServerLocation pop _)
                                      , serfRpcAddress
                                      } = do
      mSourcePoP <- originPoP addr
      case mSourcePoP of
        Nothing -> pure state
        Just sourcePoP
          | sourcePoP == pop -> do
            -- Message from our pop - distribute over trans-pop
            --_ <- logInfo "Local stream being delivered to trans-pop" { streamId: streamId }
            result <- Serf.event state.serfRpcAddress "streamAvailable" (StreamState StreamAvailable streamId addr) false
            _ <- maybeLogError "Trans-PoP serf event failed" result {}
            pure state
          | otherwise -> pure state

announceStreamStopped:: StreamId -> ServerAddress -> Effect Unit
announceStreamStopped streamId addr =
  Gen.doCast serverName ((map CastNoReply) <<< doAnnounceStreamStopped)
  where
    doAnnounceStreamStopped :: State -> Effect State
    doAnnounceStreamStopped state@{ weAreLeader: false } = pure state

    doAnnounceStreamStopped  state@{ thisLocation: (ServerLocation pop _)
                                   , serfRpcAddress
                                   } = do
      mSourcePoP <- originPoP addr
      case mSourcePoP of
        Nothing -> pure state
        Just sourcePoP
          | sourcePoP == pop -> do
            -- Message from our pop - distribute over trans-pop
            --_ <- logInfo "Local stream stopped being delivered to trans-pop" { streamId: streamId }
            result <- Serf.event state.serfRpcAddress "streamStopped" (StreamState StreamStopped streamId addr) false
            _ <- maybeLogError "Trans-PoP serf event failed" result {}
            pure state
          | otherwise -> pure state

announceTransPoPLeader :: ServerAddress -> Effect Unit
announceTransPoPLeader address =
  Gen.doCast serverName ((map CastNoReply) <<< doAnnounceTransPoPLeader)
  where
    doAnnounceTransPoPLeader :: State -> Effect State
    doAnnounceTransPoPLeader state@{ thisNode
                                   , weAreLeader: true
                                   , serfRpcAddress
                                   }
      | address < thisNode = do
        result <- Serf.leave serfRpcAddress
        _ <- logInfo "Another node has taken over as transpop leader; stepping down" { leader: address }
        stopResp <- osCmd "scripts/stopTransPoPAgent.sh"
        _ <- logInfo "Stop Resp" {resp: stopResp}

        now <- systemTimeMs
        pure
           $ state
              { currentLeader = Just address
              , lastLeaderAnnouncement = now
              , weAreLeader = false
              -- TODO - why is this type hint needed?  Does not compile without
              , members = Map.empty :: Map PoPName (Set.Set ServerAddress)
              }

    doAnnounceTransPoPLeader state@{ weAreLeader: true } =
      pure state

    doAnnounceTransPoPLeader state@{ currentLeader
                                   , thisNode
                                   }
      | Just address /= currentLeader
          && address /= thisNode = do
        _ <- logInfo "Another node has announced as transpop leader, remember which one" { leader: address }
        now <- systemTimeMs
        pure
          $ state
              { currentLeader = Just address
              , lastLeaderAnnouncement = now
              }

    doAnnounceTransPoPLeader state = do
      now <- systemTimeMs
      pure $ state { lastLeaderAnnouncement = now }

startLink :: {config :: Config.TransPoPAgentConfig, intraPoPApi :: Config.IntraPoPAgentApi} -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

init :: {config :: Config.TransPoPAgentConfig, intraPoPApi :: Config.IntraPoPAgentApi} -> Effect State
init { config: config@{ leaderTimeoutMs
                      , leaderAnnounceMs
                      , rejoinEveryMs
                      , rpcPort
                      }
     , intraPoPApi} = do
  _ <- logInfo "Trans-PoP Agent Starting" {config: config}
  _ <- Gen.registerExternalMapping serverName (\m -> TransPoPSerfMsg <$> (Serf.messageMapper m))
  _ <- Timer.sendEvery serverName leaderAnnounceMs Tick
  rpcBindIp <- Env.privateInterfaceIp
  thisNode <- PoPDefinition.thisNode
  thisLocation <- PoPDefinition.thisLocation
  now <- systemTimeMs

  let
    ServerLocation thisPoP _ = thisLocation
    serfRpcAddress =
      { ip: show rpcBindIp
      , port: rpcPort
      }

  pure
    $ { intraPoPApi
      , config: config
      , currentLeader: Nothing
      , weAreLeader: false
      , lastLeaderAnnouncement: now
      , leaderAnnouncementTimeout: wrap leaderTimeoutMs
      , thisNode: thisNode
      , thisLocation: thisLocation
      , thisPoP: thisPoP
      , serfRpcAddress: serfRpcAddress
      , members: Map.empty
      , streamStateClocks: EMap.empty
    }

shouldProcessStreamState :: StreamId -> Int -> EMap StreamId Int -> Boolean
shouldProcessStreamState streamId ltime streamStateClocks =
  case EMap.lookup streamId streamStateClocks of
    Just lastLTime
      | lastLTime > ltime -> false
    _ ->
      true

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state@{ thisPoP } = case msg of
  Tick -> CastNoReply <$> handleTick state

  JoinAll -> do
    _ <- joinAllSerf state
    pure $ CastNoReply state

  ConnectStream -> do
    _ <- connectStream state
    pure $ CastNoReply state

  TransPoPSerfMsg tmsg ->
    let
      newState =
        case tmsg of
          Serf.MemberAlive members -> membersAlive members state
          Serf.MemberLeaving -> pure state
          Serf.MemberLeft members -> membersLeft members state
          Serf.MemberFailed -> pure state
          Serf.StreamFailed -> do
            _ <- logInfo "Lost connection to TransPoP Serf Agent" {}
            unsafeCrashWith ("lost_serf_connection")
          Serf.UserEvent name ltime coalesce transMessage ->
            do
              mSourcePoP <- messageOrigin transMessage
              case mSourcePoP of
                Nothing -> pure state
                Just sourcePoP
                  | sourcePoP == thisPoP -> pure state
                  | otherwise ->
                    let
                      StreamState stateChange streamId _ = transMessage
                    in
                      case shouldProcessStreamState streamId ltime state.streamStateClocks of
                        false -> do
                          _ <- logInfo "Dropping out-of-order serf message" {streamId, stateChange}
                          pure $ state

                        true -> do
                          newStreamStateClocks <- EMap.insert' streamId ltime state.streamStateClocks
                          handleTransPoPMessage transMessage (state{streamStateClocks = newStreamStateClocks})
     in
       CastNoReply <$> newState

handleTransPoPMessage :: TransMessage -> State -> Effect State
handleTransPoPMessage (StreamState StreamAvailable streamId server) state@{intraPoPApi: {announceRemoteStreamIsAvailable}} = do
  _ <- logInfo "Remote stream available" {streamId, server}
  _ <- announceRemoteStreamIsAvailable streamId server
  pure state

handleTransPoPMessage (StreamState StreamStopped streamId server) state@{intraPoPApi: {announceRemoteStreamStopped}} = do
  _ <- logInfo "Remote stream stopped" {streamId, server}
  _ <- announceRemoteStreamStopped streamId server
  pure state

membersAlive :: (List Serf.SerfMember) -> State -> Effect State
membersAlive aliveMembers state =
  do
    _ <- logInfo "Members Alive" {members: _.name <$> aliveMembers}
    newMembers <- foldl addPoP (pure $ state.members) aliveMembers
    pure state {members = newMembers}
  where
    addPoP :: Effect (Map PoPName (Set.Set ServerAddress)) -> Serf.SerfMember -> Effect (Map PoPName (Set.Set ServerAddress))
    addPoP eMembers aliveMember@{name: aliveName} =
      do
        members <- eMembers
        mPoP <- getPoP aliveMember
        pure $ fromMaybe members ((\popName ->
                                    let
                                      existingMembers = case Map.lookup popName members of
                                                          Just existing -> existing
                                                          Nothing -> Set.empty
                                      newMembers = Set.insert (ServerAddress aliveName) existingMembers
                                    in
                                     Map.insert popName newMembers members
                                  ) <$> mPoP)

membersLeft :: (List Serf.SerfMember) -> State -> Effect State
membersLeft leftMembers state =
  do
    _ <- logInfo "Members Left" {members: _.name <$> leftMembers}
    newMembers <- foldl removePoP (pure state.members) leftMembers
    pure state {members = newMembers}
  where
    removePoP :: Effect (Map PoPName (Set.Set ServerAddress)) -> Serf.SerfMember -> Effect (Map PoPName (Set.Set ServerAddress))
    removePoP eMembers leftMember@{name: leftName} =
      do
        members <- eMembers
        mPoP <- getPoP leftMember
        pure $ fromMaybe members ((\popName ->
                                    let
                                      existingMembers = case Map.lookup popName members of
                                                          Just existing -> existing
                                                          Nothing -> Set.empty
                                      newMembers = Set.delete (ServerAddress leftName) existingMembers
                                    in
                                     Map.insert popName newMembers members
                                  ) <$> mPoP)

getPoP :: Serf.SerfMember -> Effect (Maybe PoPName)
getPoP {name : memberName} =
  do
    mServerLocation <- whereIsServer (ServerAddress memberName)
    pure $ case mServerLocation of
      Nothing -> Nothing
      Just (ServerLocation popName _) -> Just popName

handleTick :: State -> Effect State
handleTick state@{ weAreLeader: true, serfRpcAddress, thisNode, members, intraPoPApi: {announceTransPoPLeader: intraPoP_announceTransPoPLeader} } = do
  _ <- intraPoP_announceTransPoPLeader
  -- us <- Serf.getCoordinate serfRpcAddress thisNode
  -- _ <- traverse_ (\popMembers ->
  --                  do
  --                    traverse_ (\popMember ->
  --                                do
  --                                  resp <- Serf.getCoordinate serfRpcAddress popMember
  --                                  _ <- logInfo "rtt" {node: popMember,
  --                                                      rtt : (lift2 calcRtt us resp)}
  --                                  pure unit
  --                              )
  --                              popMembers
  --                )
  --      (Map.values members)
  pure state
  where
    calcRtt lhs rhs =
      let
        sumq = foldl (\acc (Tuple a b) -> acc + ((a - b) * (a - b))) 0.0 (zip lhs.vec rhs.vec)
        rtt = sqrt sumq + lhs.height + rhs.height
        adjusted = rtt + lhs.adjustment + rhs.adjustment
      in
       if adjusted > 0.0 then adjusted * 1000.0
       else rtt * 1000.0

handleTick state@{ lastLeaderAnnouncement
                 , leaderAnnouncementTimeout
                 } = do
  now <- systemTimeMs
  if (lastLeaderAnnouncement + leaderAnnouncementTimeout) < now then
    becomeLeader now state
  else do
    pure $ state

becomeLeader :: Milliseconds -> State -> Effect State
becomeLeader now state@{ lastLeaderAnnouncement
                       , leaderAnnouncementTimeout
                       , serfRpcAddress
                       , config: {connectStreamAfterMs}
                       , intraPoPApi: {announceTransPoPLeader: intraPoP_announceTransPoPLeader}
                       } = do

  _ <- logInfo "Leader is absent, becoming leader" {}
  _ <- osCmd "scripts/startTransPoPAgent.sh"
  _ <- intraPoP_announceTransPoPLeader
  _ <- Timer.sendAfter serverName connectStreamAfterMs ConnectStream
  pure
    $ state
        { lastLeaderAnnouncement = now
        , weAreLeader = true
        }

connectStream :: State -> Effect Unit
connectStream state@{ weAreLeader: false } = do
  pure unit

connectStream state@{serfRpcAddress} = do

  _ <- loopStreamJoin serfRpcAddress 5
  _ <- Timer.sendAfter serverName 0 JoinAll
  pure unit

  where
    loopStreamJoin rpcAddress n =
     do resp <- Serf.stream rpcAddress
        case resp of
            Left error ->
              case n of
                   0 -> do
                        _ <- logInfo "Could not connect to TransPoP Serf Agent" { error: error }
                        unsafeCrashWith ("could_not_connect_stream")
                   _ -> do
                         _ <- sleep (wrap 100)
                         loopStreamJoin serfRpcAddress $ n - 1
            Right x ->
              pure resp

joinAllSerf :: State -> Effect Unit
joinAllSerf state@{ weAreLeader: false } = do
  pure unit

joinAllSerf state@{ config: config@{rejoinEveryMs}, serfRpcAddress, members } =
  let
    serverAddressToSerfAddress :: String -> IpAndPort
    serverAddressToSerfAddress s =
      { ip: s
      , port: config.bindPort
      }
  in
    do
      allOtherPoPs <- PoPDefinition.getOtherPoPs
      _ <- Timer.sendAfter serverName rejoinEveryMs JoinAll

      let
        toJoin = Map.values $ Map.difference (toMap allOtherPoPs) members :: List PoP
      if length toJoin < (length allOtherPoPs) / 2 then
        pure unit
      else
        traverse_ (\{ name, servers: serversInPoP } ->
                    do
                     indexes <- distinctRandomNumbers 1 ((length serversInPoP) - 1)
                     let
                       servers :: List ServerAddress
                       servers = map (\i -> index serversInPoP i) indexes
                                 # sequence
                                 # fromMaybe nil

                     (spawnFun serverAddressToSerfAddress servers)
                  )
                  toJoin
  where
  toMap :: List PoP -> Map PoPName PoP
  toMap list = foldl (\acc pop@{name} -> Map.insert name pop acc) Map.empty list

  spawnFun :: (String -> IpAndPort) -> List ServerAddress -> Effect Unit
  spawnFun addressMapper popsToJoin = void $ spawnLink (\_ -> do
                             foldl
                               ( \iAcc (ServerAddress server) -> do
                                   restResult <- SpudGun.getText ("http://" <> server <> ":3000/api/transPoPLeader")
                                   _ <- case restResult of
                                     Nothing -> pure unit
                                     Just addr -> do
                                       result <- Serf.join serfRpcAddress ((addressMapper addr) : nil) true
                                       _ <- maybeLogError "Trans-PoP serf join failed" result { server: addr }
                                       pure unit
                                   pure unit
                               )
                               mempty
                               popsToJoin
                         )

maybeLogError :: forall a b c d e. Union b (error :: e) c => Nub c d => String -> Either e a -> Record b  -> Effect Unit
maybeLogError _ (Right _) _ = pure unit
maybeLogError msg (Left err) metadata = do
  _ <- logInfo msg (Record.merge metadata {error: err})
  pure unit

logInfo :: forall a. String -> a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.TransPoP)) : nil) } { misc: metaData })
