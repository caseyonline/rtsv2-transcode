module Rtsv2.Agents.EgestRtmpServer where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, singleton)
import Erl.Process.Raw (Pid)
import Logger as Logger
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen as Gen
import Rtsv2.Agents.EgestInstance (RegistrationResp)
import Rtsv2.Agents.EgestInstance as EgestInstance
import Rtsv2.Agents.EgestInstanceSup as EgestInstanceSup
import Rtsv2.Agents.SlotTypes (SlotConfiguration)
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Names as Names
import Rtsv2.Types (LocationResp)
import Serf (Ip)
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.JsonLd (EgestSessionStats(..), EgestRtmpSessionStats)
import Shared.Rtsv2.LlnwApiTypes (StreamEgestProtocol(..))
import Shared.Rtsv2.Stream (EgestKey(..))
import Shared.Rtsv2.Types (CanaryState(..))



foreign import startServer :: Ip -> Int -> Callbacks -> Int -> Int -> Effect Unit

foreign import startServerTls :: Ip -> Int -> Callbacks -> Int -> Int -> String -> String -> Effect Unit


type Callbacks
  = { startStream :: EffectFn1 EgestKey LocationResp
    , slotLookup :: EffectFn1 EgestKey (Maybe SlotConfiguration)
    , addClient :: EffectFn3 Pid EgestKey String RegistrationResp
    , statsUpdate :: EffectFn2 EgestKey EgestRtmpSessionStats Unit
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
  
  publicListenIp <- Env.publicListenIp
  supportListenIp <- Env.supportListenIp
  loadConfig <- Config.loadConfig
  apiConfig <- Config.llnwApiConfig
  {port, tlsPort, canaryPort, canaryTlsPort, certFile, keyFile, canaryCertFile, canaryKeyFile, nbAcceptors} <- Config.rtmpEgestConfig
  let callbacks :: CanaryState -> Callbacks
      callbacks canary = { startStream: mkEffectFn1 $ startStream loadConfig canary
                         , slotLookup: mkEffectFn1 \egestKey -> EgestInstance.getSlotConfiguration egestKey
                         , addClient: mkEffectFn3 addClient
                         , statsUpdate: mkEffectFn2 \key stats -> EgestInstance.statsUpdate key (EgestRtmpSessionStats stats)
                  }
              
  -- Public servers
  startServer publicListenIp port (callbacks Live) nbAcceptors 50000
  startServerTls publicListenIp tlsPort (callbacks Live) nbAcceptors 5000 certFile keyFile

  -- Canary servers
  startServer supportListenIp canaryPort (callbacks Canary) nbAcceptors 5000
  startServerTls supportListenIp canaryTlsPort (callbacks Canary) nbAcceptors 5000 certFile keyFile

  logInfo "Egest rtmp started" {port, tlsPort, canaryPort, canaryTlsPort}

  pure {}

  where

  startStream :: Config.LoadConfig -> CanaryState -> EgestKey -> Effect LocationResp
  startStream loadConfig canary (EgestKey slotId slotRole) =
      EgestInstanceSup.findEgest loadConfig canary slotId slotRole

  addClient :: Pid -> EgestKey -> String -> Effect RegistrationResp
  addClient pid egestKey sessionId =
    EgestInstance.addClient RtmpEgest pid egestKey sessionId


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
