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
import Rtsv2.Serf (StateMessage(..))
import Shared.Agent as Agent

type Config
  = {
    }

serverName :: ServerName State Msg
serverName = Local "transPopAgent"

data Msg = Tick
         | IntraPoPMsg StateMessage

startLink :: Config -> Effect StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

type State = { currentLeader :: Maybe ServerAddress
             , lastLeaderAnnouncement :: Int
             , leaderAnnouncementTimeout :: Int
             }

init :: Config -> Effect State
init _ = do

  _ <- Bus.subscribe serverName IntraPoPAgent.bus IntraPoPMsg
  _ <- Timer.sendEvery serverName 1000 Tick
  now <- systemTime MilliSecond

  pure $ { currentLeader : Nothing
         , lastLeaderAnnouncement : now
         , leaderAnnouncementTimeout : 2000}

handleInfo :: Msg -> State -> Effect State
handleInfo msg state =
  case msg of
    Tick -> handleTick state

    IntraPoPMsg (TransPoPLeader address) -> handleLeaderAnnouncement address state

    IntraPoPMsg a -> do
      _ <- logInfo "Got an intrapop" {msg: a}
      pure $ state

handleTick :: State -> Effect State
handleTick state@{lastLeaderAnnouncement,
                  leaderAnnouncementTimeout} =
  do
    now <- systemTime MilliSecond
    if lastLeaderAnnouncement + leaderAnnouncementTimeout < now
      then do
        _ <- logInfo "Leader is abscent" {}
        pure $ state
      else do
        _ <- logInfo "Got a leader" {}
        pure $ state

handleLeaderAnnouncement :: ServerAddress -> State -> Effect State
handleLeaderAnnouncement address state =
  do
    _ <- logInfo "Got a transpop leader announcement" {leader : address}
    now <- systemTime MilliSecond
    pure $ state { currentLeader = Just address
                 , lastLeaderAnnouncement = now}

logInfo :: forall a b. Nub ( domain :: List Atom | a ) b => String -> Record a -> Effect Foreign
logInfo msg metaData = Logger.info msg (Record.merge { domain: ((atom (show Agent.TransPoP)) : nil) } { misc: metaData })
