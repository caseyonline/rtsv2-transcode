-module(rtsv2_ingestRtmpServer@foreign).

-include_lib("id3as_media/include/rtmp.hrl").

-export([
         startServerImpl/6
        ]).

startServerImpl(Left, Right, {ipv4, O1, O2, O3, O4}, Port, NbAcceptors, Callbacks) ->
  fun() ->
      case rtmp_server:start_listener({rtmp_listener, Port},
                                      NbAcceptors,
                                      [{ip, {O1, O2, O3, O4}}, {port, Port}],
                                      [{dispatch, [{'*', rtsv2_rtmp_ingest_handler, [Callbacks]}]},
                                       {config, #rtmp_server_config{}}]) of
        {ok, _} -> Right;
        {error, {alread_started, _}} -> Right;
        Error -> Left(Error)
      end
  end.
