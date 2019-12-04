-module(serf@foreign).
-include_lib("id3as_common/include/serf_api.hrl").
-include_lib("id3as_common/include/common.hrl").

-export([ joinImpl/5
        , leaveImpl/3
        , eventImpl/6
        , streamImpl/3
        , messageMapperImpl/1
        ]).
%% event :: forall a. IpAndPort -> String -> a -> Boolean ->  Effect (Either SerfApiError Unit)
eventImpl(Left, Right, RpcAddr, Name, Msg, Coalesce) ->
  fun() ->
      case serf_api:event(mapAddr(RpcAddr),
                          Name,
                          term_to_binary(Msg),
                          Coalesce) of
        ok -> Right;

        {error, Error} ->
          ?SLOG_WARNING("serf error", #{ error => Error
                                       , client_rpc => RpcAddr
                                       , msg => Msg
                                       , coalesce => Coalesce
                                      }),
          Left(networkError)
      end
  end.

streamImpl(Left, Right, RpcAddr) ->
  fun() ->
      case serf_api:stream(mapAddr(RpcAddr)) of
        ok -> Right;
        Error -> Left(Error)
      end
  end.

joinImpl(Left, Right, RpcAddr, SeedAgents, Replay) ->
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
          Left(networkError);
        {ok, #serf_join_response{status = serf_error,
                                 serf_error = ErrorBin}} ->
          Left(ErrorBin);
        {ok, #serf_join_response{status = ok,
                                 no_peers_joined = Peers}} ->
          Right(Peers)
      end
  end.

leaveImpl(Left, Right, RpcAddr) ->
  fun() ->
      case serf_api:leave(mapAddr(RpcAddr)) of
        ok ->
          Right(unit);

        {error, Error} ->
          ?SLOG_WARNING("serf error", #{ error => Error
                                      , client_rpc => RpcAddr
                                      }),
          Left(networkError)
      end
  end.

messageMapperImpl(#serf_user_event{ name = Name
                                  , lamport_time = LTime
                                  , coalesce = Coalesce
                                  , payload = Payload
                                  }) ->
  {just, {userEvent, Name, LTime, Coalesce, binary_to_term(Payload)}};

messageMapperImpl(#serf_members_event{ type = join
                                     , members = Members
                                     }) ->
  {just, {memberAlive}};

messageMapperImpl(#serf_members_event{ type = leave
                                     , members = Members
                                     }) ->
  {just, {memberLeft}};

messageMapperImpl(_) ->
  {nothing}.

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
