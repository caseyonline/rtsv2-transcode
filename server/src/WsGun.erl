-module(wsGun@foreign).

-export([ openImpl/1
        , upgradeImpl/2
        , sendImpl/2
        , messageMapperImpl/1
        ]).

openImpl(Url) ->
  fun() ->
      {ok, {_Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(binary_to_list(Url)),

      ConnectTimeout = 5000,

      case gun:open(Host, Port, #{ connect_timeout => ConnectTimeout
                                 , retry => 0
                                 , supervise => false
                                 , ws_opts => #{ compress => true
                                               , silence_pings => true
                                               }
                                 }) of
        {ok, ConnPid} ->
          {right, {ConnPid, Path ++ Query}};

        {error, Reason} ->
          {left, Reason}
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

messageMapperImpl({gun_up, ConnPid, Protocol}) ->
  {just, {gunUp, ConnPid, map_protocol(Protocol)}};

messageMapperImpl({gun_down, ConnPid, Protocol, Reason, KilledStreams, UnprocessedStreams}) ->
  {just, {gunDown, ConnPid, map_protocol(Protocol), Reason, KilledStreams, UnprocessedStreams}};

messageMapperImpl({gun_upgrade, ConnPid, StreamRef, _Protocols, Headers}) ->
  {just, {gunUpgrade, ConnPid, StreamRef, Headers}};

messageMapperImpl({gun_ws, ConnPid, StreamRef, Frame}) ->
  {just, {gunWsFrame, ConnPid, StreamRef, map_frame(Frame)}};

messageMapperImpl({gun_error, ConnPid, Reason}) ->
  {just, {gunWsConnectionError, ConnPid, Reason}};

messageMapperImpl({gun_error, ConnPid, StreamRef, Reason}) ->
  {just, {gunWsStreamError, ConnPid, StreamRef, Reason}};

messageMapperImpl(_) ->
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
