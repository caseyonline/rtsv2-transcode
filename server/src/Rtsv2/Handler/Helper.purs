module Rtsv2.Handler.Helper
       ( webSocketHandler
       , WebSocketHandlerResult(..)
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Cowboy.Handlers.WebSocket (Frame(..))
import Erl.Cowboy.Req (Req)
import Erl.Data.List (singleton)
import Foreign (Foreign)
import Rtsv2.Agents.StreamRelayTypes (WebSocketHandlerMessage)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Stetson (InnerStetsonHandler, WebSocketCallResult(..))
import Stetson.WebSocket as WebSocket

foreign import webSocketMsgMapperImpl :: Foreign -> Maybe WebSocketHandlerMessage

data WebSocketHandlerResult serverMsg state = WebSocketNoReply state
                                            | WebSocketReply serverMsg state
                                            | WebSocketStop state

webSocketHandler :: forall clientMsg serverMsg state. ReadForeign clientMsg => WriteForeign serverMsg =>
                    (Req -> Effect state) ->
                    (state -> Effect (WebSocketHandlerResult serverMsg state)) ->
                    (state -> clientMsg -> Effect (WebSocketHandlerResult serverMsg state)) ->
                    (state -> WebSocketHandlerMessage -> Effect (WebSocketHandlerResult serverMsg state)) ->
                    InnerStetsonHandler WebSocketHandlerMessage state
webSocketHandler init wsInit handle info =
  WebSocket.handler (\req -> do
                        state <- init req
                        WebSocket.initResult req state
                    )

  # WebSocket.init (\router state -> do
                     res <- wsInit state
                     pure $ dispatchResult res
                   )

  # WebSocket.handle (\rawFrame state ->
                       case rawFrame of
                         TextFrame str ->
                           case readJSON str of
                             Left _ ->
                               -- todo - what about parse failures?  Should crash?
                               pure $ NoReply state
                             Right frame -> do
                               res <- handle state frame
                               pure $ dispatchResult res
                         BinaryFrame bin ->
                           pure $ NoReply state
                         PingFrame bin ->
                           pure $ NoReply state
                         PongFrame bin ->
                           pure $ NoReply state
                     )

  # WebSocket.info (\msg state -> do
                       res <- info state msg
                       pure $ dispatchResult res
                   )

  # WebSocket.externalMapping webSocketMsgMapperImpl

  # WebSocket.yeeha
  where
    dispatchResult res =
      case res of
        WebSocketNoReply state ->
          NoReply state
        WebSocketReply response state ->
          let
            str = writeJSON response
          in
            Reply (singleton (TextFrame str)) state
        WebSocketStop state ->
          Stop state
