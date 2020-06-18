module Rtsv2.Top
       ( startLink
       , stop
       )
       where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List, singleton)
import Logger as Logger
import Pinto (ServerName, ok)
import Pinto as Pinto
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Rtsv2.Names as Names
import Rtsv2.Sup as Sup

type State =
  {
  }

------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------
startLink :: Effect Pinto.StartLinkResult
startLink = Gen.startLink serverName init Gen.defaultHandleInfo

stop :: Effect Unit
stop = Gen.doCall serverName doStop
  where
    doStop state = do
      logInfo "Top terminating Sup" {}
      Sup.stop
      logInfo "Top terminating" {}
      pure $ CallReply unit state

------------------------------------------------------------------------------
-- gen-server callbacks
------------------------------------------------------------------------------
init :: Effect State
init = do
  ok =<< Sup.startLink
  pure { }

------------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------------
serverName :: ServerName State Unit
serverName = Names.topName

domain :: List Atom
domain = serverName # Names.toDomain # singleton

logInfo :: forall report. String -> { | report } -> Effect Unit
logInfo = Logger.info <<< Logger.traceMetadata domain
