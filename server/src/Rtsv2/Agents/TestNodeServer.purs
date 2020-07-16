module Rtsv2.Agents.TestNodeServer
       (
         startLink
       , transcode
       )
       where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Function.Uncurried (Fn2, Fn5, mkFn2, mkFn5)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Logger as Logger
import Media.Rtmp as Rtmp
import Media.SourceDetails as SourceDetails
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Rtsv2.Agents.IngestInstance (getPublishCredentials, getStreamAuthType, getStreamDetails)
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Agents.IngestRtmpCrypto (AdobeContextParams, AdobePhase1Params, AdobePhase2Params, LlnwContextParams, LlnwPhase1Params, LlnwPhase2Params, Phase2Params(..), checkCredentials)
import Rtsv2.Agents.IngestRtmpCrypto as IngestRtmpCrypto
import Rtsv2.Config (LoadConfig)
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Names as Names
import Rtsv2.PoPDefinition as PoPDefinition
import Rtsv2.Types (LocalResource(..))
import Rtsv2.Utils (crashIfLeft, noprocToMaybe)
import Serf (Ip)
import Shared.Common (ProfileContext)
import Shared.Rtsv2.Agent as Agent
import Shared.Rtsv2.LlnwApiTypes (AuthType, SlotProfile(..), SlotPublishAuthType(..), StreamDetails, StreamIngestProtocol(..))
import Shared.Rtsv2.Stream (IngestKey(..))
import Shared.Rtsv2.Types (CanaryState(..), extractAddress)
import Stetson.WebSocketHandler (self)
import Effect.Console (log, logShow)
import Stetson (StetsonHandler)
import Shared.Common (Url)
import StetsonHelper (jsonResponse)
import Effect.Console (log)

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------

serverName :: ServerName State Unit
serverName = Names.testNodeServerName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink args = Gen.startLink serverName (init args) Gen.defaultHandleInfo


transcode :: VideoTitle -> VideoBitrate -> Effect State
transcode videoTitle videoBitrate =
  Gen.doCall serverName \state@{} -> do
      logInfo "----------------- Transcode ---------" {text : videoTitle}
      pure $ CallReply {} state


type MyRecord = {value :: Int,
                text :: String}

type VideoTitle = String
type VideoBitrate = String

type State =
  {
  }

data Msg
   = TestInfo

------------------------------------------------------------------------------
-- callbacks
------------------------------------------------------------------------------

init :: forall a. a -> Effect State
init _ = do
  logInfo "----------------- TestNodeServer: init running ---------" {value : 6900}
  pure $ {}

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = do
  case msg of
    TestInfo -> do
      logInfo "----------------- blah blah blah ------------------------------" {value : 8600}
      pure $ CastNoReply state

------------------------------------------------------------------------------
-- helper
------------------------------------------------------------------------------
domain :: List Atom
domain = atom <$> (show Agent.TestNode :  "Instance" : nil)

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo msg = Logger.info (Logger.traceMetadata domain msg)



