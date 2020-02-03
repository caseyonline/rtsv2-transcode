module Rtsv2.Handler.IntraPoP
       ( leader
       , publicState
       ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.Newtype (unwrap)
import Erl.Data.List (singleton)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.IntraPoP as IntraPoPAgent
import Rtsv2.Handler.MimeType as MimeType
import Shared.Types (Server, extractAddress)
import Shared.Types.Agent.State as PublicState
import Stetson (StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GenericStetsonGet, genericGet)


leader:: StetsonHandler (Maybe Server)
leader =
  Rest.handler init
  # Rest.resourceExists resourceExists
  # Rest.contentTypesProvided (\req state -> Rest.result (singleton $ MimeType.text provideText) req state)
  # Rest.yeeha
  where
    init req = do
      currentLeader <- IntraPoPAgent.currentTransPoPLeader
      Rest.initResult req currentLeader

    resourceExists req state = do
      Rest.result (isJust state) req state

    provideText req state = do
      Rest.result (fromMaybe "" (unwrap <<< extractAddress <$> state)) req state


publicState :: GenericStetsonGet PublicState.IntraPoP
publicState = genericGet (\_ -> IntraPoP.getPublicState)
