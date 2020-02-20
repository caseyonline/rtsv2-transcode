module Rtsv2.Agents.IngestRtmpServer
       (
         startLink
       )
       where

import Prelude

import Data.Either (Either(..), hush)
import Data.Function.Uncurried (Fn3, Fn5, mkFn3, mkFn5)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (uncons)
import Erl.Process.Raw (Pid)
import Foreign (Foreign, unsafeToForeign)
import Logger (spy)
import Media.Rtmp as Rtmp
import Media.SourceDetails as SourceDetails
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen as Gen
import Rtsv2.Agents.IngestInstance as IngestInstance
import Rtsv2.Agents.IngestInstanceSup as IngestInstanceSup
import Rtsv2.Agents.IngestRtmpCrytpo (AdobeContextParams, CryptoContext(..), LlnwContextParams)
import Rtsv2.Agents.IngestRtmpCrytpo as IngestRtmpCrypto
import Rtsv2.Config as Config
import Rtsv2.Env as Env
import Rtsv2.Names as Names
import Rtsv2.Utils (crashIfLeft)
import Serf (Ip)
import Shared.LlnwApiTypes (AuthType, PublishCredentials(..), SlotPublishAuthType(..), StreamAuth, StreamConnection, StreamDetails, StreamIngestProtocol(..), StreamPublish)
import Shared.Stream (IngestKey(..))
import SpudGun (bodyToJSON)
import SpudGun as SpudGun
import Stetson.WebSocketHandler (self)

foreign import startServerImpl :: (Foreign -> Either Foreign Unit) -> Either Foreign Unit -> Ip -> Int -> Int -> Callbacks -> Effect (Either Foreign Unit)
foreign import rtmpQueryToPurs :: Foreign -> RtmpAuthRequest
foreign import compareAdobeChallengeImpl :: String -> String -> String -> String -> String -> String -> Boolean
foreign import compareLlnwChallengeImpl :: String -> String -> String -> String -> String -> String -> String -> Boolean
foreign import startWorkflowImpl :: Pid -> Foreign -> IngestKey -> (Foreign -> (Effect Unit)) -> (Foreign -> (Effect Unit)) -> Effect Unit

type AdobePhase1Params =
  { username :: String
  }

type AdobePhase2Params =
  { username :: String
  , clientChallenge :: String
  , clientResponse :: String
  }

type LlnwPhase1Params =
  { username :: String
  }

type LlnwPhase2Params =
  { username :: String
  , shortname :: String
  , clientNonce :: String
  , clientNc :: String
  , clientResponse :: String
  }

data RtmpAuthRequest = Initial
                     | AdobePhase1 AdobePhase1Params
                     | AdobePhase2 AdobePhase2Params
                     | LlnwPhase1 LlnwPhase1Params
                     | LlnwPhase2 LlnwPhase2Params

data RtmpAuthResponse = InitialResponse AuthType
                      | AdobePhase1Response String AdobeContextParams
                      | LlnwPhase1Response LlnwContextParams
                      | RejectRequest
                      | AcceptRequest (Fn5 String Int String Pid Foreign (Effect Unit))

data Phase2Params = AdobePhase2P AdobePhase2Params
                  | LlnwPhase2P LlnwPhase2Params

type Callbacks
  = { init :: Fn3 String String Foreign (Effect RtmpAuthResponse)
    }

serverName :: ServerName State Unit
serverName = Names.ingestRtmpServerName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink args = Gen.startLink serverName (init args) Gen.defaultHandleInfo


type State =
  {
  }

init :: forall a. a -> Effect State
init _ = do
  interfaceIp <- Env.publicInterfaceIp
  {port, nbAcceptors} <- Config.rtmpIngestConfig
  {streamAuthTypeUrl, streamAuthUrl, streamPublishUrl} <- Config.llnwApiConfig
  let
    callbacks :: Callbacks
    callbacks = { init: mkFn3 handlerInit
                }
  crashIfLeft =<< startServerImpl Left (Right unit) interfaceIp port nbAcceptors callbacks
  pure $ {}

handlerInit :: String -> String -> Foreign -> (Effect RtmpAuthResponse)
handlerInit host shortname foreignQuery =
  let
    authRequest = rtmpQueryToPurs foreignQuery
  in
    processAuthRequest host shortname authRequest

processAuthRequest :: String -> String -> RtmpAuthRequest -> Effect RtmpAuthResponse
processAuthRequest host shortname Initial = do
  authType <- getStreamAuthType host shortname
  pure $ fromMaybe RejectRequest $ InitialResponse <$> authType

processAuthRequest host shortname (AdobePhase1 {username}) = do
  context <- IngestRtmpCrypto.newAdobeContext username
  pure $ AdobePhase1Response username context

processAuthRequest host shortname (LlnwPhase1 {username}) = do
  context <- IngestRtmpCrypto.newLlnwContext username
  pure $ LlnwPhase1Response context

processAuthRequest host shortname (AdobePhase2 authParams@{username}) =
  processAuthRequest' host shortname username (AdobePhase2P authParams)

processAuthRequest host shortname (LlnwPhase2 authParams@{username}) =
  processAuthRequest' host shortname username (LlnwPhase2P authParams)

handlerHandle :: String -> String -> String -> String -> Int -> String -> Pid -> Foreign -> Effect Unit
handlerHandle host shortname username remoteAddress remotePort streamName rtmpPid publishArgs = do
  let
    streamPublish = { host
                    , protocol: Rtmp
                    , shortname
                    , streamName
                    , username
                    }
  maybeStreamDetails <- getStreamDetails streamPublish
  case maybeStreamDetails of
    Nothing ->
      pure unit

    Just streamDetails -> do
      let
        ingestKey = makeIngestKey streamName streamDetails
      self <- self
      IngestInstanceSup.startIngest ingestKey streamPublish streamDetails remoteAddress remotePort self
      startWorkflowAndBlock rtmpPid publishArgs ingestKey
      IngestInstance.stopIngest ingestKey
      pure unit
  where
    makeIngestKey streamVariantId {role, slot: {name: streamId}} =
      IngestKey (wrap streamId) role (wrap streamVariantId)

processAuthRequest' :: String -> String -> String -> Phase2Params -> Effect RtmpAuthResponse
processAuthRequest' host shortname username authParams = do
  let
    authType = case authParams of
                 AdobePhase2P _ -> Adobe
                 LlnwPhase2P _ -> Llnw
  cryptoContexts <- IngestRtmpCrypto.getCryptoContexts authType username
  maybePublishCredentials <- getPublishCredentials host shortname username
  let
    -- mapmap :: forall f g a b. Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
    -- mapmap = (<$>) <<< (<$>)
    result :: Effect RtmpAuthResponse
    result = maybePublishCredentials
             >>= checkCredentials authParams cryptoContexts
             # traverse consumeContext
             <#> join
             <#> (\a -> const (AcceptRequest (mkFn5 (handlerHandle host shortname username))) <$> a)
             <#> fromMaybe RejectRequest
  result

  where
    checkCredentials params cryptoContexts credentials =
      case uncons cryptoContexts of
        Nothing -> Nothing
        Just {head, tail} ->
          case compareChallenge params head credentials of
            true -> Just head
            false -> checkCredentials params tail credentials

    compareChallenge (AdobePhase2P params) (AdobeContext context) credentials =
      compareAdobeChallenge params context credentials

    compareChallenge (LlnwPhase2P params) (LlnwContext context) credentials =
      compareLlnwChallenge params context credentials

    compareChallenge _ _ _ =
      unsafeCrashWith "fail"

    consumeContext :: CryptoContext -> Effect (Maybe CryptoContext)
    consumeContext context = do
      consumed <- IngestRtmpCrypto.consumeCryptoContext username context
      pure $ if consumed then Just context
             else Nothing


startWorkflowAndBlock :: Pid -> Foreign -> IngestKey -> Effect Unit
startWorkflowAndBlock rtmpPid publishArgs ingestKey =
  startWorkflowImpl rtmpPid publishArgs ingestKey clientMetadata sourceInfo
  where
    clientMetadata foreignMetadata = do
      IngestInstance.setClientMetadata ingestKey (Rtmp.foreignToMetadata foreignMetadata)

    sourceInfo foreignSourceInfo = do
      IngestInstance.setSourceInfo ingestKey (SourceDetails.foreignToSourceInfo foreignSourceInfo)


getStreamAuthType :: String -> String -> Effect (Maybe AuthType)
getStreamAuthType host shortname = do
  {streamAuthTypeUrl: url} <- Config.llnwApiConfig
  restResult <- SpudGun.postJson (wrap url) (spy "authtype body" { host
                                                                 , protocol: Rtmp
                                                                 , shortname} :: StreamConnection
                                                          )
  pure $ hush (bodyToJSON (spy "authtype result" restResult))

getPublishCredentials :: String -> String -> String -> Effect (Maybe PublishCredentials)
getPublishCredentials host shortname username = do
  {streamAuthUrl: url} <- Config.llnwApiConfig
  restResult <- SpudGun.postJson (wrap (spy "auth url" url)) (spy "auth body" { host
                                                                              , shortname
                                                                              , username} :: StreamAuth
                                                             )
  pure $ hush $ bodyToJSON (spy "auth result" restResult)

getStreamDetails :: StreamPublish -> Effect (Maybe StreamDetails)
getStreamDetails streamPublish = do
  {streamPublishUrl: url} <- Config.llnwApiConfig
  restResult <- SpudGun.postJson (wrap (spy "publish url" url)) (spy "publish body" streamPublish)
  pure $ hush (spy "publish parse" (bodyToJSON (spy "publish result" restResult)))

compareAdobeChallenge :: AdobePhase2Params -> AdobeContextParams -> PublishCredentials -> Boolean
compareAdobeChallenge {username, clientChallenge, clientResponse} {salt, challenge} (PublishCredentials {username: expectedUsername, password}) =
  compareAdobeChallengeImpl expectedUsername salt password challenge clientChallenge clientResponse

compareLlnwChallenge :: LlnwPhase2Params -> LlnwContextParams -> PublishCredentials -> Boolean
compareLlnwChallenge {username, shortname, clientNonce, clientNc, clientResponse} {nonce} (PublishCredentials {username: expectedUsername, password}) =
  compareLlnwChallengeImpl expectedUsername password shortname nonce clientNc clientNonce clientResponse
