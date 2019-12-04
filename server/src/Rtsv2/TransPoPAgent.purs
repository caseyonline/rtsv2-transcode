module Rtsv2.TransPoPAgent where

import Prelude

import Bus as Bus
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Process (spawnLink)
import Erl.Utils (systemTime, TimeUnit(..))
import Foreign (Foreign)
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Prim.Row (class Nub)
import Record as Record
import Rtsv2.Env as Env
import Rtsv2.IntraPoPAgent as IntraPoPAgent
import Rtsv2.PoPDefinition (ServerAddress, ServerLocation(..), PoP, whereIsServer)
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Serf (StateMessage(..), IpAndPort)
import Rtsv2.Serf as Serf
import Serf as Serf
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
  | IntraPoPMsg StateMessage

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
            } =
  do
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
handleInfo msg state = case msg of
  Tick -> handleTick state
  JoinAll -> joinAllSerf state
  IntraPoPMsg (TransPoPLeader address) -> handleLeaderAnnouncement address state
  IntraPoPMsg (StreamAvailable streamId addr) -> handleIntraPoPStreamAvaiable streamId addr state
  IntraPoPMsg a -> do
    -- TODO - if we are leader and get intra-pop-streamAvailable then publish to trans-pop
    _ <- logInfo "Got an intrapop" { msg: a }
    pure $ state

--TransPopMsg a ->
-- TODO - if we are leader and transpop-streamAvailable from other PoP, then call IntraPop.streamIsAvailable
handleIntraPoPStreamAvaiable :: StreamId -> ServerAddress -> State -> Effect State
handleIntraPoPStreamAvaiable _ _ state@{ weAreLeader: false } = pure state

handleIntraPoPStreamAvaiable streamId addr state@{ thisLocation: (ServerLocation pop _) } = do
  _ <- logInfo "got a popStreaAvaible msg" { streamId, addr }
  mServerLocation <- whereIsServer addr
  _ <- logInfo "location is " { mServerLocation }
  case mServerLocation of
    Nothing -> pure state
    Just (ServerLocation sourcePoP _)
      | sourcePoP == pop -> pure state
      | otherwise -> do
        _ <- IntraPoPAgent.announceStreamIsAvailable streamId
        pure state

handleTick :: State -> Effect State
handleTick state@{ weAreLeader: true } = do
  _ <- IntraPoPAgent.announceTransPoPLeader
  pure state

handleTick state@{ lastLeaderAnnouncement
                 , leaderAnnouncementTimeout
                 } = do
  now <- systemTime MilliSecond
  if lastLeaderAnnouncement + leaderAnnouncementTimeout < now then do
    _ <- logInfo "Leader is absent, becoming leader" {}
    _ <- IntraPoPAgent.announceTransPoPLeader
    pure
      $ state
          { lastLeaderAnnouncement = now
          , weAreLeader = true
          }
  else do
    pure $ state

handleLeaderAnnouncement :: ServerAddress -> State -> Effect State
handleLeaderAnnouncement address state@{ thisNode
                                       , weAreLeader: true
                                       , serfRpcAddress
                                       }
  | address < thisNode = do
    result <- Serf.leave serfRpcAddress
    _ <- logInfo "Another node has taken over as transpop leader; stepping down" { leader: address }
    now <- systemTime MilliSecond
    pure
      $ state
          { currentLeader = Just address
          , lastLeaderAnnouncement = now
          , weAreLeader = false
          }

handleLeaderAnnouncement address state@{ currentLeader
                                       , thisNode
                                       , weAreLeader: true
                                       }
  | Just address /= currentLeader
      && address
      /= thisNode = do
    _ <- logInfo "Another node has announced as transpop leader, but we remain leader" { leader: address }
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
joinAllSerf state@{ weAreLeader : false } =
  do
    _ <- Timer.sendAfter serverName 4000 JoinAll
    pure state
    
joinAllSerf state@{ config, serfRpcAddress } =
  let
    serverAddressToSerfAddress :: String -> IpAndPort
    serverAddressToSerfAddress s = {ip: s,
                                    port: config.bindPort}
  in
  do
    -- TODO - could spawn a process per seed and issue the joins in parallel
    pops <- PoPDefinition.getSeedsForOtherPoPs :: Effect (List PoP)
    _ <- sequence $ foldl (\acc {name, servers} ->
                            (spawnLink (\_ ->
                                         do
                                           foldl (\iAcc server ->
                                                   do
                                                     restResult <- SpudGun.getText ("http://" <> server <> ":3000/poc/api/transPoPLeader")
                                                     _ <- case restResult of
                                                            Nothing ->
                                                              pure unit
                                                            Just addr ->
                                                              do
                                                                result <- Serf.join serfRpcAddress ((serverAddressToSerfAddress addr) : nil) true
                                                                _ <- logInfo "Rest said " { server: addr
                                                                                          , result: result }
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
