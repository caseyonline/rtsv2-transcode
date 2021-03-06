-module(wsGun@foreign).

-include_lib("id3as_common/include/common.hrl").

-export([ openImpl/2
        , closeImpl/1
        , upgradeImpl/2
        , sendImpl/2
        , messageMapperImpl/1
        , '$handle_undefined_function'/2
        ]).

'$handle_undefined_function'(Function, Args) ->
  ?SLOG_INFO("HERE", #{ '_function' => Function
                      , args => Args}),
  ok.

openImpl(Url, KeepAlive) ->
  fun() ->
      {ok, {_Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(binary_to_list(Url)),

      ConnectTimeout = 5000,

      case gun:open(Host, Port, #{ connect_timeout => ConnectTimeout
                                 , retry => 0
                                 %%, supervise => false
                                 , ws_opts => #{ compress => true
                                               , silence_pings => false
                                               , keepalive => KeepAlive
                                               }
                                 %%, event_handler => {'wsGun@foreign', ok}
                                 }) of
        {ok, ConnPid} ->
          {right, {ConnPid, Path ++ Query}};

        {error, Reason} ->
          {left, Reason}
      end
  end.

closeImpl(ConnPid) ->
  fun() ->
      case gun:close(ConnPid) of
        ok -> io:format(user, "Socket ~p closed~n", [ConnPid]), ok;
        {error, not_found} -> io:format(user, "Socket ~p not found~n", [ConnPid]), ok
      end
  end.

sendImpl(Msg, ConnPid) ->
  fun() ->
      gun:ws_send(ConnPid, {text, Msg})
  end.

upgradeImpl(ConnPid, Path) ->
  fun() ->
      Ref = gun:ws_upgrade(ConnPid, Path),
      Ref
  end.

messageMapperImpl(X) ->
  messageMapperImpl_(X).

messageMapperImpl_({gun_up, ConnPid, Protocol}) ->
  {just, {gunUp, ConnPid, map_protocol(Protocol)}};

%% gun 2.0 clause
messageMapperImpl_({gun_down, ConnPid, Protocol, Reason, KilledStreams}) ->
  {just, {gunDown, ConnPid, map_protocol(Protocol), Reason, KilledStreams, []}};

%% pre-gun 2.0 clause
messageMapperImpl_({gun_down, ConnPid, Protocol, Reason, KilledStreams, UnprocessedStreams}) ->
  {just, {gunDown, ConnPid, map_protocol(Protocol), Reason, KilledStreams, UnprocessedStreams}};

messageMapperImpl_({gun_upgrade, ConnPid, StreamRef, _Protocols, Headers}) ->
  {just, {gunUpgrade, ConnPid, StreamRef, Headers}};

messageMapperImpl_({gun_ws, ConnPid, StreamRef, Frame}) ->
  {just, {gunWsFrame, ConnPid, StreamRef, map_frame(Frame)}};

messageMapperImpl_({gun_error, ConnPid, Reason}) ->
  {just, {gunWsConnectionError, ConnPid, Reason}};

messageMapperImpl_({gun_error, ConnPid, StreamRef, Reason}) ->
  {just, {gunWsStreamError, ConnPid, StreamRef, Reason}};

messageMapperImpl_(_Other) ->
  {nothing}.

map_frame(close) ->
  {close, {nothing}, {nothing}};

map_frame(ping) ->
  {ping, {nothing}};

map_frame(pong) ->
  {pong, {nothing}};

map_frame({text, Text}) ->
  {text, Text};

map_frame({binary, Binary}) ->
  {binary, Binary};

map_frame({close, Binary}) ->
  {close, {nothing}, {just, Binary}};

map_frame({close, Int, Binary}) ->
  {close, {just, Int}, {just, Binary}};

map_frame({ping, Binary}) ->
  {ping, {just, Binary}};

map_frame({pong, Binary}) ->
  {pong, {just, Binary}}.

map_protocol(http) ->
  {hTTP};
map_protocol(http2) ->
  {hTTP2};
map_protocol(socks) ->
  {socks};
map_protocol(ws) ->
  {webSocket}.
