-module(rtsv2_handler_helper@foreign).

-export([
         webSocketMsgMapperImpl/1
        ]).

webSocketMsgMapperImpl({'DOWN', _Ref, process, _, _Reason}) ->
  {just, {'wsStop'}};

webSocketMsgMapperImpl(_) ->
  {nothing}.
