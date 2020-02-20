module Rtsv2.Agents.IngestRtmpCrytpo
       ( startLink
       , newContext
       , getCryptoContexts
       , consumeCryptoContext
       , CryptoContext(..)
       , AdobeContextParams
       , LlnwContextParams
       , QueryContextParams
       )
       where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Erl.Data.List (List, filter, nil, null, (:))
import Erl.Data.Map (Map, alter, insert, lookup)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, fst, snd, tuple2)
import Erl.Utils (vmTimeMs)
import Partial.Unsafe (unsafeCrashWith)
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.Utils (cryptoStrongToken, member)
import Shared.LlnwApiTypes (SlotPublishAuthType(..))
import Shared.Types (Milliseconds)

type Username = String

data CryptoContext = AdobeContext AdobeContextParams
                   | LlnwContext LlnwContextParams
                   | QueryContext QueryContextParams

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

newContext :: SlotPublishAuthType -> Username -> Effect CryptoContext
newContext Adobe username =
  Gen.doCall serverName \state -> do
    salt <- cryptoStrongToken 6
    challenge <- cryptoStrongToken 6
    let
      context = {salt, challenge}
    state2 <- addContextToStateAndReply username context adobeLens state
    pure $ Gen.CallReply (AdobeContext context) state2

newContext Llnw username =
  Gen.doCall serverName \state -> do
    nonce <- cryptoStrongToken 6
    let
      context = {nonce}
    state2 <- addContextToStateAndReply username context llnwLens state
    pure $ Gen.CallReply (LlnwContext context) state2

newContext Query username =
  unsafeCrashWith "query auth type not implemented"

getCryptoContexts :: SlotPublishAuthType -> Username -> Effect (List CryptoContext)
getCryptoContexts Adobe username =
  Gen.call serverName \state ->
   Gen.CallReply (AdobeContext <$> (getContexts'' username adobeLens state)) state

getCryptoContexts Llnw username =
  Gen.call serverName \state ->
   Gen.CallReply (LlnwContext <$> (getContexts'' username llnwLens state)) state

getCryptoContexts Query username =
  unsafeCrashWith "query auth type not implemented"

consumeCryptoContext :: Username -> CryptoContext -> Effect Boolean
consumeCryptoContext username (AdobeContext consumedContext) =
  Gen.call serverName (consumeContext username consumedContext adobeLens)

consumeCryptoContext username (LlnwContext consumedContext) =
  Gen.call serverName (consumeContext username consumedContext llnwLens)

consumeCryptoContext username (QueryContext consumedContext) =
  unsafeCrashWith "query auth type not implemented"

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

getContexts'' :: forall a. Username -> ContextLens a -> State -> (List a)
getContexts'' username {get} state =
  get state # getContexts' username

getContexts :: forall a. Username -> ContextLens a -> State -> CallResult (List a) State
getContexts username {get} state =
  let
    result = get state
             # getContexts' username
  in
   CallReply result state

getContexts' :: forall a. Username -> Map Username (ContextsWithExpiry a) -> List a
getContexts' username contexts =
  lookup username contexts
  # fromMaybe nil
  <#> snd

consumeContext :: forall a. Eq a => Username -> a -> ContextLens a -> State -> CallResult Boolean State
consumeContext username consumedContext {get, set} state =
  let
    contexts = get state

    currentUserContexts = getContexts' username contexts

    removeContext Nothing = Nothing
    removeContext (Just contextsWithExpiry) =
      let
        newContextsWithExpiry :: List (ContextWithExpiry a)
        newContextsWithExpiry = filter (\contextWithExpiry -> (snd contextWithExpiry) /= consumedContext) contextsWithExpiry
      in
       if null newContextsWithExpiry then Nothing
       else Just newContextsWithExpiry

    newContexts = alter removeContext username contexts
  in
   if
     (member consumedContext currentUserContexts) then CallReply true (set newContexts state)
     else CallReply false state


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
