module Helpers.Node where

import Prelude

import Data.Array (catMaybes, filter, intercalate)
import Data.Time.Duration (Milliseconds(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, delay)
import Helpers.Types (Node(..), NodeAddress(..))
import Helpers.Env (sessionName)
import Helpers.CreateString (mkPoPJsonString, toAddrFromNode)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import OsCmd (runProc)
import Shared.Types (ServerAddress(..))

stopNode :: Node -> Aff Unit
stopNode node = do
  let nodeAddr = toAddrFromNode node
  runProc "./scripts/stopNode.sh" [ sessionName
                                  , nodeAddr
                                  , nodeAddr
                                  ]
startSession :: Array Node -> Aff Unit
startSession allNodes = do
  stopSession
  writeTextFile UTF8 "config/popDefinition.json" $ mkPoPJsonString allNodes
  runProc "./scripts/startSession.sh" [sessionName]

stopSession :: Aff Unit
stopSession = do
  _ <- delay (Milliseconds 200.0)
  runProc "./scripts/stopSession.sh" [sessionName]
