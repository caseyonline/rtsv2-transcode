-module(rtsv2_agents_egestRtmpServer@foreign).

-include_lib("id3as_media/include/rtmp.hrl").

-export([startServer/5]).

startServer(PublicIp, Port, Callbacks, NbAcceptors, ReceiveTimeout) ->
  fun() ->
    case start_rtmp_server(PublicIp, Port, NbAcceptors, ReceiveTimeout, Callbacks, false, []) of
      ok -> ok;
      {error, Error} -> throw({rtmp_start_error, Error})
    end
  end.

start_rtmp_server({ipv4, O1, O2, O3, O4}, Port, NbAcceptors, ReceiveTimeout, Callbacks, UseTls, Opts) ->
  case rtmp_server:start_listener({rtmp_listener, Port},
                                  NbAcceptors,
                                  [{ip, {O1, O2, O3, O4}}, {port, Port}] ++ Opts,
                                  [{dispatch, [{'*', rtsv2_rtmp_egest_handler, [Callbacks]}]},
                                   {config, #rtmp_server_config{receive_loop_timeout = ReceiveTimeout, ssl = UseTls}}]) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok;
    Error -> {error, Error}
  end.