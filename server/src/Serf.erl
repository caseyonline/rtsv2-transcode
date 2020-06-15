-module(serf@foreign).

-include_lib("id3as_common/include/serf_api.hrl").
-include_lib("id3as_common/include/common.hrl").

-export([ membersImpl/3
        , joinImpl/5
        , leaveImpl/3
        , eventImpl/6
        , streamImpl/3
        , getCoordinateImpl/4
        , messageMapperImpl/1
        ]).

%% eventImpl :: (ApiError -> (SerfResult Unit)) -> (SerfResult Unit) -> IpAndPort -> String -> Binary -> Boolean ->  Effect (SerfResult Unit)
eventImpl(Left, Right, RpcAddr, Name, Msg, Coalesce) ->
  fun() ->
      case serf_api:event(mapAddr(RpcAddr),
                          Name,
                          Msg,
                          Coalesce) of
        ok -> Right;

        {error, Error} ->
          ?SLOG_WARNING("serf event error", #{ error => Error
                                             , client_rpc => RpcAddr
                                             , msg => Msg
                                             , coalesce => Coalesce
                                             }),
          map_error(Error, Left)
      end
  end.

membersImpl(Left, Right, RpcAddr) ->
  fun() ->
      case serf_api:members(mapAddr(RpcAddr)) of
        {ok, #serf_members_response{members = Members}} ->
          Right([mapMemberToPurs(Member) || Member <- Members]);

        {error, Error} ->
          ?SLOG_WARNING("serf members error", #{ error => Error
                                               , client_rpc => RpcAddr
                                               }),
          map_error(Error, Left)
      end
  end.

streamImpl(Left, Right, RpcAddr) ->
  fun() ->
      case serf_api:stream(mapAddr(RpcAddr)) of
        ok -> Right;
        {error, Error} -> map_error(Error, Left)
      end
  end.

getCoordinateImpl(Left, Right, RpcAddr, NodeName) ->
  fun() ->
      case serf_api:get_coordinate(mapAddr(RpcAddr), NodeName) of
        {ok, Coords} -> Right(mapCoordsToPurs(Coords));
        {error, Error} -> map_error(Error, Left)
      end
  end.

joinImpl(Left, Right, RpcAddr, SeedAgents, Replay) ->
  fun() ->
      case serf_api:join(mapAddr(RpcAddr),
                         [mapAddr(Seed) || Seed <- SeedAgents],
                         Replay) of
        {ok, #serf_join_response{no_peers_joined = Peers}} ->
          Right(Peers);

       {error, Error} ->
          ?SLOG_WARNING("serf join error", #{ error => Error
                                            , client_rpc => RpcAddr
                                            , seeds => SeedAgents
                                            , replay_events => Replay
                                            }),
          map_error(Error, Left)
      end
  end.

leaveImpl(Left, Right, RpcAddr) ->
  fun() ->
      case serf_api:leave(mapAddr(RpcAddr)) of
        ok ->
          Right(unit);

        {error, Error} ->
          ?SLOG_WARNING("serf leave error", #{ error => Error
                                             , client_rpc => RpcAddr
                                             }),
          map_error(Error, Left)
      end
  end.

messageMapperImpl(#serf_user_event{ name = Name
                                  , lamport_time = LTime
                                  , coalesce = Coalesce
                                  , payload = Payload
                                  }) ->
  {just, {userEvent, Name, LTime, Coalesce, Payload}};

messageMapperImpl(#serf_members_event{ type = join
                                     , members = Members
                                     }) ->
  {just, {memberAlive, [mapMemberToPurs(Member) || Member <- Members]}};

messageMapperImpl(#serf_members_event{ type = leave
                                     , members = Members
                                     }) ->
  {just, {memberLeft, [mapMemberToPurs(Member) || Member <- Members]}};

messageMapperImpl(#serf_coordinates_response{ adjustment = Adjustment
                                            , error = Error
                                            , height = Height
                                            , vec = Vec}) ->
  {just, #{adjustment => Adjustment
          , error => Error
          , height => Height
          , vec => Vec}};

%% TODO - should be a record
messageMapperImpl({serf_stream_failed, _Ref, _Error}) ->
  {just, {streamFailed}};

messageMapperImpl(_X) ->
  {nothing}.


mapCoordsToPurs(#serf_coordinates_response{ adjustment = Adjustment
                                          , error = Error
                                          , height = Height
                                          , vec = Vec}) ->
  #{adjustment => Adjustment
   , error => Error
   , height => Height
   , vec => Vec}.

mapMemberToPurs(#serf_member{ name = Name
                            , ip_address = Addr
                            , port = Port
                            , tags = Tags
                            , status = Status
                            , api_protocol_version_min = ProtocolMin
                            , api_protocol_version_max = ProtocolMax
                            , api_protocol_version_cur = ProtocolCur
                            , api_delegate_version_min = DelegateMin
                            , api_delegate_version_max = DelegateMax
                            , api_delegate_version_cur = DelegateCur
                            }) ->
  #{name => Name
   , ip_address => serf_api:addr_to_binary_string(Addr)
   , port => Port
   , tags => Tags
   , status => Status
   , api_protocol_version_min => ProtocolMin
   , api_protocol_version_max => ProtocolMax
   , api_protocol_version_cur => ProtocolCur
   , api_delegate_version_min => DelegateMin
   , api_delegate_version_max => DelegateMax
   , api_delegate_version_cur => DelegateCur
   }.

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

map_error({serfError, Error}, Left) ->
  Left({serfError, Error});

map_error(Error, Left) ->
  Left({networkError, Error}).
