module Rtsv2.Agents.IngestRtmpCrytpo
       ( startLink
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
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Config as Config
import Rtsv2.Names as Names
import Rtsv2.Utils (cryptoStrongToken, member)
import Shared.Types (Milliseconds)

type Username = String

type AdobeContext =
  { salt :: String
  , challenge :: String
  }

type LlnwContext =
  { nonce :: String
  }

type ContextWithExpiry a = Tuple2 Milliseconds a
type ContextsWithExpiry a = List (ContextWithExpiry a)

type ContextLens a =
  { get :: State -> Map Username (ContextsWithExpiry a)
  , set :: Map Username (ContextsWithExpiry a) -> State -> State
  }

type State =
  { adobeContexts :: Map Username (ContextsWithExpiry AdobeContext)
  , llnwContexts ::  Map Username (ContextsWithExpiry LlnwContext)
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

newAdobeContext :: Username -> Effect AdobeContext
newAdobeContext username =
  Gen.doCall serverName \state -> do
    salt <- cryptoStrongToken 6
    challenge <- cryptoStrongToken 6
    let
      context = {salt, challenge}
    addContextToStateAndReply username context adobeLens state

newLlnwContext :: Username -> Effect LlnwContext
newLlnwContext username =
  Gen.doCall serverName \state@{llnwContexts} -> do
    nonce <- cryptoStrongToken 6
    let
      context = {nonce}
    addContextToStateAndReply username context llnwLens state

getAdobeContexts :: Username -> Effect (List AdobeContext)
getAdobeContexts username =
  Gen.call serverName (getContexts username adobeLens)

getLlnwContexts :: Username -> Effect (List LlnwContext)
getLlnwContexts username =
  Gen.call serverName (getContexts username llnwLens)

consumeAdobeContext :: Username -> AdobeContext -> Effect Boolean
consumeAdobeContext username consumedContext =
  Gen.call serverName (consumeContext username consumedContext adobeLens)

consumeLlnwContext :: Username -> LlnwContext -> Effect Boolean
consumeLlnwContext username consumedContext =
  Gen.call serverName (consumeContext username consumedContext llnwLens)

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

addContextToStateAndReply :: forall a. Username -> a -> ContextLens a -> State -> Effect (CallResult a State)
addContextToStateAndReply username context {get, set} state@{cryptoContextExpiry} = do
  now <- vmTimeMs
  let
    contextWithExpiry = tuple2 (now + cryptoContextExpiry) context
    contexts = get state
    newContexts = alter (addToUserContexts contextWithExpiry) username contexts
  pure $ Gen.CallReply context (set newContexts state)
  where
    addToUserContexts contextWithExpiry Nothing = Just (contextWithExpiry : nil)
    addToUserContexts contextWithExpiry (Just existing) = Just (contextWithExpiry : existing)

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


adobeLens :: ContextLens AdobeContext
adobeLens =
  { get: _.adobeContexts
  , set: \newAdobeContexts state -> state{adobeContexts = newAdobeContexts}
  }

llnwLens :: ContextLens LlnwContext
llnwLens =
  { get: _.llnwContexts
  , set: \newLlnwContexts state -> state{llnwContexts = newLlnwContexts}
  }
