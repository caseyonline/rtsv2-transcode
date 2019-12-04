module Rtsv2.TransPoPAgent where

import Prelude

import Bus as Bus
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Utils (systemTime, TimeUnit(..))
import Foreign (Foreign)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub)
import Record as Record
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.PoPDefinition (ServerAddress)
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Serf (StateMessage(..))
import Shared.Agent as Agent

type Config = { bindPort :: Int
              , rpcPort :: Int
              , leaderTimeoutMs :: Int
              , leaderAnnounceMs :: Int
              }

serverName :: ServerName State Msg
serverName = Local "transPopAgent"

data Msg = Tick
         | IntraPoPMsg StateMessage

startLink :: Config -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

type State = { currentLeader :: Maybe ServerAddress
             , weAreLeader :: Boolean
             , lastLeaderAnnouncement :: Int
             , leaderAnnouncementTimeout :: Int
             , thisNode :: ServerAddress
             }

init :: Config -> Effect State
init {leaderTimeoutMs,
      leaderAnnounceMs} = do

  _ <- Bus.subscribe serverName IntraPoPAgent.bus IntraPoPMsg
  _ <- Timer.sendEvery serverName leaderAnnounceMs Tick
  thisNode <- PoPDefinition.thisNode
  now <- systemTime MilliSecond

  pure $ { currentLeader : Nothing
         , weAreLeader : false
         , lastLeaderAnnouncement : now
         , leaderAnnouncementTimeout : leaderTimeoutMs
         , thisNode : thisNode
         }

handleInfo :: Msg -> State -> Effect State
handleInfo msg state =
  case msg of
    Tick -> handleTick state

    IntraPoPMsg (TransPoPLeader address) -> handleLeaderAnnouncement address state

    IntraPoPMsg a -> do
      -- TODO - if we are leader and transpop-streamAvailable from other PoP, then call IntraPop.streamIsAvailable
      -- TODO - if we are leader and get intra-pop-streamAvailable then publish to trans-pop
      _ <- logInfo "Got an intrapop" {msg: a}
      pure $ state

handleTick :: State -> Effect State
handleTick state@{weAreLeader : true} =
  do
    _ <- IntraPoPAgent.announceTransPoPLeader
    pure state

handleTick state@{lastLeaderAnnouncement,
                  leaderAnnouncementTimeout} =
  do
    now <- systemTime MilliSecond
    if lastLeaderAnnouncement + leaderAnnouncementTimeout < now
      then do
        _ <- logInfo "Leader is absent, becoming leader" {}
        _ <- IntraPoPAgent.announceTransPoPLeader
        pure $ state { lastLeaderAnnouncement = now,
                       weAreLeader = true}
      else do
        pure $ state

handleLeaderAnnouncement :: ServerAddress -> State -> Effect State
handleLeaderAnnouncement address state@{thisNode,
                                        weAreLeader : true} | address < thisNode =
  do
    _ <- logInfo "Another node has taken over as transpop leader; stepping down" {leader : address}
    now <- systemTime MilliSecond
    pure $ state { currentLeader = Just address
                 , lastLeaderAnnouncement = now
                 , weAreLeader = false
                 }

handleLeaderAnnouncement address state@{currentLeader,
                                        thisNode} | Just address /= currentLeader &&
                                                    address /= thisNode =
  do
    _ <- logInfo "Another node has announced as transpop leader, but we remain leader" {leader : address}
    now <- systemTime MilliSecond
    pure $ state { currentLeader = Just address
                 , lastLeaderAnnouncement = now}

handleLeaderAnnouncement address state =
  do
    now <- systemTime MilliSecond
    pure $ state { lastLeaderAnnouncement = now}

logInfo :: forall a b. Nub ( domain :: List Atom | a ) b => String -> Record a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.TransPoP)) : nil) } { misc: metaData })
