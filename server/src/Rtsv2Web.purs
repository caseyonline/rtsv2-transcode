module Rtsv2Web
  ( startLink
  , init
  , serverName
  , State
  )
  where

import Prelude

import Shared.Agents as Agents
import Effect (Effect)
import Erl.Cowboy.Req (Req)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import Pinto (ServerName(..), StartLinkResult)
import Pinto.Gen as Gen
import Stetson (RestResult, StetsonHandler)
import Stetson as Stetson
import Stetson.Rest as Rest
import Gproc as Gproc

newtype State = State {}

type Rtsv2WebStartArgs = { webPort :: Int }

serverName :: ServerName State
serverName = ServerName "web"

startLink :: Rtsv2WebStartArgs -> Effect StartLinkResult
startLink args =
  Gen.startLink serverName $ init args

init :: Rtsv2WebStartArgs -> Effect State
init args = do
  Stetson.configure
    # Stetson.route "/poc/api/client/:canary/:stream_id" edge_entrypoint
    # Stetson.route "/test/alive" alive_entrypoint
    # Stetson.port args.webPort
    # Stetson.bindTo 0 0 0 0
    # Stetson.startClear "http_listener"
  pure $ State {}



alive_entrypoint :: StetsonHandler Unit
alive_entrypoint =
  Rest.handler (\req -> Rest.initResult req unit)
  # Rest.contentTypesProvided (\req state -> Rest.result (emptyText : nil) req unit)
  # Rest.yeeha


edge_entrypoint :: StetsonHandler String
edge_entrypoint =
  Rest.handler (\req -> Rest.initResult req "state")
  # Rest.serviceAvailable (\req state -> do
                              registered <- Gproc.isRegistered Agents.EdgeAgent
                              Rest.result registered req state)
  # Rest.contentTypesProvided (\req state -> Rest.result (textWriter : nil) req state)
  # Rest.yeeha


emptyText  :: forall a. Tuple2 String (Req -> a -> (Effect (RestResult String a)))
emptyText = tuple2 "text/plain" (\req state -> Rest.result "" req state)

textWriter :: Tuple2 String (Req -> String -> (Effect (RestResult String String)))
textWriter = tuple2 "text/plain" (\req state -> Rest.result state req state)
