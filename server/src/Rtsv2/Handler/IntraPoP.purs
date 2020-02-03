module Rtsv2.Handler.IntraPoP
       ( leader
       , state
       ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (tuple2)
import Rtsv2.Agents.IntraPoP as IntraPoP
import Rtsv2.Agents.IntraPoP as IntraPoPAgent
import Shared.Types (ServerAddress)
import Shared.Types.Agent.State as PublicState
import Stetson (StetsonHandler)
import Stetson.Rest as Rest
import StetsonHelper (GenericStetsonGet, genericGet)


leader:: StetsonHandler (Maybe ServerAddress)
leader =
  Rest.handler (\req -> Rest.initResult req Nothing)
  # Rest.resourceExists (\req state -> do
                            currentLeader <- IntraPoPAgent.currentTransPoPLeader
                            Rest.result (isJust currentLeader) req currentLeader
                          )
  # Rest.contentTypesProvided (\req state ->
                                Rest.result ((tuple2 "text/plain" (\req2 currentLeader -> Rest.result (fromMaybe "" (unwrap <$> currentLeader)) req2 state)) : nil) req state)
  # Rest.yeeha


state :: GenericStetsonGet PublicState.IntraPoP
state= genericGet (\_ -> IntraPoP.getPublicState)
