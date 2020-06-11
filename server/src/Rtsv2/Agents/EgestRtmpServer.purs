module Rtsv2.Agents.EgestRtmpServer where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, singleton)
import Logger as Logger
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen as Gen
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Names as Names
import Rtsv2.Types (LocationResp)
import Serf (Ip)
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.Stream (EgestKey(..))
import Shared.Rtsv2.Types (CanaryState(..))



foreign import startServer :: Ip -> Int -> Callbacks -> Int -> Int -> Effect Unit


type Callbacks
  = { startStream :: EffectFn1 EgestKey LocationResp
    }

serverName :: ServerName State Unit
serverName = Names.egestRtmpServerName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink args = Gen.startLink serverName (init args) Gen.defaultHandleInfo


type State =
  {
  }

init :: forall a. a -> Effect State
init _ = do
  logInfo "Egest rtmp starting" {port: 5123}
  publicListenIp <- Env.publicListenIp
  loadConfig <- Config.loadConfig
  let callbacks :: Callbacks
      callbacks = { startStream: mkEffectFn1 $ startStream loadConfig Live
                  }
              
  startServer publicListenIp 5123 callbacks 10 30000
  pure {}

  where

  startStream :: Config.LoadConfig -> CanaryState -> EgestKey -> Effect LocationResp
  startStream loadConfig canary (EgestKey slotId slotRole) =
      EgestInstanceSup.findEgest loadConfig canary slotId slotRole

--------------------------------------------------------------------------------
-- Log helpers
--------------------------------------------------------------------------------
domain :: List Atom
domain = Agent.Egest # show # atom # singleton

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo = Logger.info <<< Logger.traceMetadata domain

logStart :: forall report. String -> { | report } -> Effect Unit
logStart = Logger.info <<< Logger.eventMetadata domain Logger.Start

logStop :: forall report. String -> { | report } -> Effect Unit
logStop = Logger.info <<< Logger.eventMetadata domain Logger.Stop
