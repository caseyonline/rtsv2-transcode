-module(wsGun@foreign).

-export([ openImpl/1
        , upgradeImpl/2
        , sendImpl/2
        , pingImpl/1
        , messageMapperImpl/1
        ]).

openImpl(Url) ->
  fun() ->
      {ok, {_Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(binary_to_list(Url)),

      ConnectTimeout = 5000,
      PingPeriod = 1000,

      {ok, ConnPid} = gun:open(Host, Port, #{ connect_timeout => ConnectTimeout
                                            , ws_opts => #{compress => true}
                                            }),

      {ok, TRef} = timer:send_interval(PingPeriod, {wsgun, ping}),

      {right, {gunState, #{ connPid => ConnPid
                          , path => Path ++ Query
                          , tref => TRef
                          }}}

      %% case gun:await_up(ConnPid, ConnectTimeout) of
      %%   {error, timeout} -> {left, {error, connect_timeout}};
      %%   {error, UpOther} -> {left, {error, UpOther}};
      %%   {ok, _Protocol} ->
      %%     _Ref = gun:ws_upgrade(ConnPid, Path ++ Query),
      %%     {right, ConnPid}
      %% end
  end.

sendImpl(ConnPid, Msg) ->
  fun() ->
      gun:ws_send(ConnPid, {text, Msg})
  end.

pingImpl(ConnPid) ->
  fun() ->
      try
        gun:ws_send(ConnPid, {ping, <<0>>})
      catch
        _:_ -> ok
      end
  end.

upgradeImpl(ConnPid, Path) ->
  fun() ->
      Ref = gun:ws_upgrade(ConnPid, Path),
      Ref
  end.

messageMapperImpl({wsgun, ping}) ->
  {just, {gunWsSendPing}};

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
