module Rtsv2IntraPoPAgent where

import Prelude

import Effect (Effect)
import Erl.Data.List (List, nil, (:))
import Erl.Atom (Atom, atom)
import Foreign (Foreign)
import Gproc as Gproc
import Logger as Logger
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen (CallResult(..))
import Pinto.Gen as Gen
import Prim.Row (class Nub)
import Record as Record
import Shared.Agents as Agents

type State
  = {}

type IntraPoPAgentStartArgs
  = {
    }

serverName :: ServerName State
serverName = ServerName "intraPopAgent"

startLink :: IntraPoPAgentStartArgs -> Effect StartLinkResult
startLink args = Gen.startLink serverName $ init args

init :: IntraPoPAgentStartArgs -> Effect State
init state = do
  _ <- logInfo "Intra-PoP Agent Starting" {foo: "bar"}
  pure $ {}

logInfo :: forall a b. Nub (domain :: List Atom | a) b =>  String -> Record a -> Effect Foreign
logInfo msg metaData =
  Logger.info msg (Record.merge {domain : ((atom (show Agents.IntraPoPAgent)): nil)} metaData)
