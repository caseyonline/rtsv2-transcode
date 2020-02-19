module Rtsv2.Agents.IngestRtmpCrytpo
       (
         startLink
       )
       where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Data.List (List, filter, nil, null, (:))
import Erl.Data.Map (Map, alter, lookup)
import Erl.Data.Map as Map
import Erl.Utils (vmTimeMs)
import Pinto (ServerName)
import Pinto as Pinto
import Pinto.Gen (CallResult(..), CastResult(..))
import Pinto.Gen as Gen
import Pinto.Timer as Timer
import Rtsv2.Names as Names
import Rtsv2.Utils (cryptoStrongToken, member)
import Shared.Types (Milliseconds)

serverName :: ServerName State Msg
serverName = Names.ingestRtmpCryptoName

startLink :: forall a. a -> Effect Pinto.StartLinkResult
startLink args = Gen.startLink serverName (init args) handleInfo

getSaltAndChallenge :: String -> Effect (Tuple String String)
getSaltAndChallenge username =
  Gen.doCall serverName \state@{contexts} -> do
    salt <- cryptoStrongToken 6
    challenge <- cryptoStrongToken 6
    now <- vmTimeMs
    let
      context = SaltAndChallenge salt challenge
      contextWithExpiry = Tuple now context
      newContexts = alter (addToContext contextWithExpiry) username contexts
    pure $ CallReply (Tuple salt challenge) state{contexts = newContexts}

getNonce :: String -> Effect String
getNonce username =
  Gen.doCall serverName \state@{contexts} -> do
    nonce <- cryptoStrongToken 6
    now <- vmTimeMs
    let
      context = Nonce nonce
      contextWithExpiry = Tuple now context
      newContexts = alter (addToContext contextWithExpiry) username contexts
    pure $ CallReply nonce state{contexts = newContexts}

getContexts :: String -> Effect (List Context)
getContexts username =
  Gen.call serverName \state@{contexts} ->
  let
    response = lookup username contexts
               # fromMaybe nil
              <#> (\(Tuple _ context) -> context)
  in
   CallReply response state

consumeContext :: String -> Context -> Effect Boolean
consumeContext username consumedContext =
  Gen.call serverName \state@{contexts} ->
  let
    currentUserContexts = lookup username contexts
                          # fromMaybe nil
                          <#> (\(Tuple _ context) -> context)

    removeContext Nothing = Nothing
    removeContext (Just contextsWithExpiry) =
      let
        newContextsWithExpiry :: List ContextWithExpiry
        newContextsWithExpiry = filter (\(Tuple _ context) -> context /= consumedContext) contextsWithExpiry
      in
       if null newContextsWithExpiry then Nothing
       else Just newContextsWithExpiry


    newContexts = alter removeContext username contexts
  in
   if
     (member consumedContext currentUserContexts) then CallReply true state{contexts = newContexts}
     else CallReply false state

data Context = SaltAndChallenge String String
             | Nonce String

derive instance eqContext :: Eq Context

type ContextWithExpiry = Tuple Milliseconds Context

type UserContexts = List ContextWithExpiry

type State =
  {
    contexts :: Map String UserContexts
  }

data Msg =
  CheckExpiry

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------
init :: forall a. a -> Effect State
init _ = do
  void $ Timer.sendAfter serverName 1000 CheckExpiry
  pure $ { contexts: Map.empty
         }

handleInfo :: Msg -> State -> Effect (CastResult State)
handleInfo msg state = case msg of
  CheckExpiry ->
    pure $ CastNoReply state

addToContext :: ContextWithExpiry -> Maybe UserContexts -> Maybe UserContexts
addToContext contextWithExpiry Nothing = Just (contextWithExpiry : nil)
addToContext contextWithExpiry (Just existing) = Just (contextWithExpiry : existing)
