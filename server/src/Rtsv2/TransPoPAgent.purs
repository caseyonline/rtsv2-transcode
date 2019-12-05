module Rtsv2.TransPoPAgent where

import Prelude

import Bus as Bus
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
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
import Rtsv2.PoPDefinition (ServerAddress, ServerLocation(..), PoP, whereIsServer)
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
    }

init :: Config -> Effect State
init config@{ leaderTimeoutMs
            , leaderAnnounceMs
            , rpcPort
            } = do
  _ <- Gen.registerExternalMapping serverName (\m -> TransPoPMsg <$> (TransSerf.messageMapper m))
  _ <- Bus.subscribe serverName IntraPoPAgent.bus IntraPoPMsg
  _ <- Timer.sendEvery serverName leaderAnnounceMs Tick
  _ <- Timer.sendAfter serverName 4000 JoinAll
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

-- handleTransPoPMessage (TransSerf.MembersAlive members) state =
--   do
--     _ <- logInfo "Members Alive" {members: members}
--     pure state
-- handleTransPoPMessage (TransSerf.MembersLeft members) state =
--   do
--     _ <- logInfo "Members Left" {members: members}
--     pure state
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
     do      _ <- sleep 300
             resp <- TransSerf.stream rpcAddress
             case resp of
                 Left _ ->
                   case n of
                        0 -> pure resp
                        _ -> loopStreamJoin serfRpcAddress $ n - 1
                 Right x -> pure resp


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
  _ <- Timer.sendAfter serverName 4000 JoinAll
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
                                      logInfo "Rest said "
                                        { server: addr
                                        , result: result
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
