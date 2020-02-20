module Rtsv2.Agents.IngestRtmpCrypto
       ( startLink
       , newAdobeContext
       , newLlnwContext
       , checkCredentials
       , AdobeContextParams
       , LlnwContextParams
       , QueryContextParams
       , Phase2Params(..)
       , AdobePhase1Params
       , AdobePhase2Params
       , LlnwPhase1Params
       , LlnwPhase2Params
       )
       where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Data.List (List, filter, nil, null, uncons, (:))
import Erl.Data.Map (Map, alter, delete, insert, lookup)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, fst, snd, tuple2)
import Erl.Utils (vmTimeMs)
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen (CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.Utils (cryptoStrongToken)
import Shared.LlnwApiTypes (PublishCredentials(..))
import Shared.Types (Milliseconds)

foreign import compareAdobeChallengeImpl :: String -> String -> String -> String -> String -> String -> Boolean
foreign import compareLlnwChallengeImpl :: String -> String -> String -> String -> String -> String -> String -> Boolean

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

data Phase2Params = AdobePhase2P AdobePhase2Params
                  | LlnwPhase2P LlnwPhase2Params

type Username = String

type AdobeContextParams =
  { salt :: String
  , challenge :: String
  }

type LlnwContextParams =
  { nonce :: String
  }

type QueryContextParams =
  {
  }

type ContextWithExpiry a = Tuple2 Milliseconds a
type ContextsWithExpiry a = List (ContextWithExpiry a)

type ContextLens a =
  { get :: State -> Map Username (ContextsWithExpiry a)
  , set :: Map Username (ContextsWithExpiry a) -> State -> State
  }

type State =
  { adobeContexts :: Map Username (ContextsWithExpiry AdobeContextParams)
  , llnwContexts ::  Map Username (ContextsWithExpiry LlnwContextParams)
  , cryptoContextExpiry :: Milliseconds
  }

data Msg =
  CheckExpiry

serverName :: ServerName State Msg
serverName = Names.ingestRtmpCryptoName

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

newAdobeContext :: Username -> Effect AdobeContextParams
newAdobeContext username =
  Gen.doCall serverName \state -> do
    salt <- cryptoStrongToken 6
    challenge <- cryptoStrongToken 6
    let
      context = {salt, challenge}
    state2 <- addContextToStateAndReply username context adobeLens state
    pure $ Gen.CallReply context state2

newLlnwContext :: Username -> Effect LlnwContextParams
newLlnwContext username =
  Gen.doCall serverName \state -> do
    nonce <- cryptoStrongToken 6
    let
      context = {nonce}
    state2 <- addContextToStateAndReply username context llnwLens state
    pure $ Gen.CallReply context state2

checkCredentials :: String -> String -> String -> PublishCredentials -> Phase2Params -> Effect Boolean
checkCredentials host shortname username credentials (AdobePhase2P authParams) =
  Gen.call serverName \state@{adobeContexts: contexts} ->
    let
      currentUserContexts = fromMaybe nil $ lookup username contexts
      result = checkCredentials' compareAdobeChallenge authParams currentUserContexts credentials nil
    in
     case result of
       Nothing -> Gen.CallReply false state
       Just newUserContexts -> Gen.CallReply true if null newUserContexts then state{adobeContexts = delete username contexts}
                                                  else state{adobeContexts = insert username newUserContexts contexts}

checkCredentials host shortname username credentials (LlnwPhase2P authParams) =
  Gen.call serverName \state@{llnwContexts: contexts} ->
    let
      currentUserContexts = fromMaybe nil $ lookup username contexts
      result = checkCredentials' compareLlnwChallenge authParams currentUserContexts credentials nil
    in
     case result of
       Nothing -> Gen.CallReply false state
       Just newUserContexts -> Gen.CallReply true if null newUserContexts then state{llnwContexts = delete username contexts}
                                                  else state{llnwContexts = insert username newUserContexts contexts}

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------
init :: forall a. a -> Effect State
init _ = do
  {cryptoContextExpiryMs} <- Config.rtmpIngestConfig
  void $ Timer.sendAfter serverName cryptoContextExpiryMs CheckExpiry
  pure $ { adobeContexts: Map.empty
         , llnwContexts: Map.empty
         , cryptoContextExpiry: wrap cryptoContextExpiryMs
         }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = case msg of
  CheckExpiry -> do
    state2 <- expireOldContexts state
    pure $ CastNoReply state2

expireOldContexts :: State -> Effect State
expireOldContexts state@{adobeContexts, llnwContexts} = do
  now <- vmTimeMs
  let
    expireOld :: forall a. Username -> Map Username (ContextsWithExpiry a) -> ContextsWithExpiry a -> Map Username (ContextsWithExpiry a)
    expireOld username acc userContexts =
      let
        newUserContexts = filter (\contextWithExpiry -> (fst contextWithExpiry) > now) userContexts
      in
       if null newUserContexts then acc
       else insert username newUserContexts acc

    adobeContexts2 = foldlWithIndex expireOld Map.empty adobeContexts
    llnwContexts2 = foldlWithIndex expireOld Map.empty llnwContexts
  pure state{ adobeContexts = adobeContexts2
            , llnwContexts = llnwContexts2}

addContextToStateAndReply :: forall a. Username -> a -> ContextLens a -> State -> Effect State
addContextToStateAndReply username context {get, set} state@{cryptoContextExpiry} = do
  now <- vmTimeMs
  let
    contextWithExpiry = tuple2 (now + cryptoContextExpiry) context
    contexts = get state
    newContexts = alter (addToUserContexts contextWithExpiry) username contexts
  pure $ set newContexts state
  where
    addToUserContexts contextWithExpiry Nothing = Just (contextWithExpiry : nil)
    addToUserContexts contextWithExpiry (Just existing) = Just (contextWithExpiry : existing)

adobeLens :: ContextLens AdobeContextParams
adobeLens =
  { get: _.adobeContexts
  , set: \newAdobeContexts state -> state{adobeContexts = newAdobeContexts}
  }

llnwLens :: ContextLens LlnwContextParams
llnwLens =
  { get: _.llnwContexts
  , set: \newLlnwContexts state -> state{llnwContexts = newLlnwContexts}
  }

checkCredentials' :: forall a params context credentials. (params -> context -> credentials -> Boolean) -> params -> List (Tuple2 a context) -> credentials -> List (Tuple2 a context) -> Maybe (List (Tuple2 a context))
checkCredentials' compareFun params cryptoContexts credentials acc =
  case uncons cryptoContexts of
    Nothing -> Nothing
    Just {head, tail} ->
      case compareFun params (snd head) credentials of
        true -> Just (tail <> acc)
        false -> checkCredentials' compareFun params tail credentials (head : acc)

compareAdobeChallenge :: AdobePhase2Params -> AdobeContextParams -> PublishCredentials -> Boolean
compareAdobeChallenge {username, clientChallenge, clientResponse} {salt, challenge} (PublishCredentials {username: expectedUsername, password}) =
  compareAdobeChallengeImpl expectedUsername salt password challenge clientChallenge clientResponse

compareLlnwChallenge :: LlnwPhase2Params -> LlnwContextParams -> PublishCredentials -> Boolean
compareLlnwChallenge {username, shortname, clientNonce, clientNc, clientResponse} {nonce} (PublishCredentials {username: expectedUsername, password}) =
  compareLlnwChallengeImpl expectedUsername password shortname nonce clientNc clientNonce clientResponse
