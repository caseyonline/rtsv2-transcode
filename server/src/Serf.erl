-module(serf@foreign).
-include_lib("id3as_common/include/serf_api.hrl").
-include_lib("id3as_common/include/common.hrl").

-export([ joinImpl/3
        ]).

joinImpl(RpcAddr
        , SeedAgents
        , Replay
        ) ->
  fun() ->
      io:format(user, "~n~nXXX ~p, ~p, ~p~n~n", [mapAddr(RpcAddr),
                                                 [mapAddr(Seed) || Seed <- SeedAgents],
                                                 Replay]),
      case serf_api:join(mapAddr(RpcAddr),
                         [mapAddr(Seed) || Seed <- SeedAgents],
                         Replay) of
        {error, Error} ->
          ?SLOG_WARNING("serf error", #{ error => Error
                                      , client_rpc => RpcAddr
                                      , seeds => SeedAgents
                                      , replay_events => Replay
                                      }),
          {left, networkError};
        {ok, #serf_join_response{status = serf_error,
                                 serf_error = ErrorBin}} ->
          {left, {serfError, ErrorBin}};
        {ok, #serf_join_response{status = ok,
                                 no_peers_joined = Peers}} ->
          {right, Peers}
      end
  end.


mapAddr(#{ ip := {ipv4, O1, O2, O3, O4}
         , port := Port
         }) ->
  {{O1, O2, O3, O4}, Port}.
