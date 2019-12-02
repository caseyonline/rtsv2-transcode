-module(serf@foreign).
-include_lib("id3as_common/include/serf_api.hrl").
-include_lib("id3as_common/include/common.hrl").

-export([ joinImpl/3
        , eventImpl/4
        , streamImpl/3
        , messageMapperImpl/1
        ]).
%% event :: forall a. IpAndPort -> String -> a -> Boolean ->  Effect (Either SerfApiError Unit)
eventImpl(RpcAddr
         , Name
         , Msg
         , Coalesce
         ) ->
  fun() ->
      io:format(user, "~n~nXXX ~p, ~p, ~p~n~n", [mapAddr(RpcAddr)
                                                , Name
                                                , Coalesce]),
      case serf_api:event(mapAddr(RpcAddr),
                          Name,
                          base64:encode(term_to_binary(Msg)),
                          Coalesce) of
        {error, Error} ->
          ?SLOG_WARNING("serf error", #{ error => Error
                                       , client_rpc => RpcAddr
                                       , msg => Msg
                                       , coalesce => Coalesce
                                      }),
          {left, networkError};
        ok -> {right, unit}
      end
  end.

streamImpl(Left, Right, RpcAddr) ->
  fun() ->
      case serf_api:stream(mapAddr(RpcAddr)) of
        ok -> Right;
        Error -> Left(Error)
      end
  end.

joinImpl(RpcAddr
        , SeedAgents
        , Replay
        ) ->
  fun() ->
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

messageMapperImpl(#serf_user_event{ name = Name
                                  , lamport_time = LTime
                                  , coalesce = Coalesce
                                  , payload = Payload
                                  }) ->
  {userEvent, Name, LTime, Coalesce, binary_to_term(base64:decode(Payload))};

messageMapperImpl(#serf_members_event{ type = join
                                     , members = Members
                                     }) ->
  {memberAlive};

messageMapperImpl(#serf_members_event{ type = leave
                                     , members = Members
                                     }) ->
  {memberLeft}.

mapAddr(#{ ip := {ipv4, O1, O2, O3, O4}
         , port := Port
         }) ->
  {{O1, O2, O3, O4}, Port};

mapAddr(#{ ip := {atom, localhost}
         , port := Port
         }) ->
  {localhost, Port};

mapAddr(#{ ip := Addr
         , port := Port
         }) when is_binary(Addr) ->
  {binary_to_list(Addr), Port}.
