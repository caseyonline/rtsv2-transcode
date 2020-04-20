-module(rtsv2_webrtc_push_ingest_ws_resource).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_rtc/include/webrtc.hrl").
-include("../rtsv2_types.hrl").

-export([ init/2 ]).
-export([ websocket_handle/2
        , websocket_info/2
        ]).

-record(pending, {}).

-record(connected,
        { session_id :: binary()
        , webrtc_session_ref :: reference()
        , stream_details :: term()
        }).

-record(state,
        { me :: binary_string()
        , remote_host :: binary_string()
        , remote_port :: non_neg_integer()
        , account :: binary_string()
        , stream_name :: binary_string()
        , state = #pending{} :: #pending{} | #connected{}
        , last_activity = i_timestamp:utc_now() :: erlang:timestamp()
        , publish_stream :: fun()
        }).

init(Req, #{ publish_stream := PublishStream }) ->

  RemoteHost = cowboy_req:host(Req),
  RemotePort = cowboy_req:port(Req),
  Account = cowboy_req:binding(account, Req),
  StreamName = cowboy_req:binding(stream_name, Req),

  Me = cowboy_req:host(Req),
  {cowboy_websocket, Req, #state{ me = Me
                                , remote_host = RemoteHost
                                , remote_port = RemotePort
                                , account = Account
                                , stream_name = StreamName
                                , publish_stream = PublishStream }}.

websocket_handle({text, JSON}, State = #state{state = InnerState}) ->
  case jsx:decode(JSON, [return_maps]) of
    Map = #{ <<"type">> := Type } ->
      MaybeData = maps:get(<<"data">>, Map, undefined),
      case handle_message(Type, MaybeData, InnerState, State) of
        {ok, NewInnerState} ->
          {ok, update_state(State, NewInnerState)};
        {reply, Reply, NewInnerState} ->
          {reply, {text, jsx:encode(Reply)}, update_state(State, NewInnerState)}
      end
  end.

handle_message(<<"join">>, Data, _InnerState = #pending{}, _OuterState = #state{ account = Account
                                                                               , stream_name = StreamName
                                                                               , remote_host = RemoteHost
                                                                               , remote_port = RemotePort
                                                                               , publish_stream = PublishStream
                                                                               }) ->

  case jsx:decode(Data, [return_maps]) of
    #{ <<"username">> := Username
     , <<"password">> := Password } ->

      case (PublishStream(Account, Username, Password, RemoteHost, RemotePort, StreamName))() of

        {just, StreamDetails} ->
io:format(user, "GOT JOIN ~p~n", [StreamDetails]),
          ?INFO("Got stream details! ~p", [StreamDetails]),
          SessionId = generate_session_id(),
          webrtc_session:subscribe_for_msgs(SessionId, [#webrtc_session_response{}]),
          Response = make_response(<<"join">>, SessionId),
          {reply, Response, #connected{ session_id = SessionId
                                      , stream_details = StreamDetails}};
        {nothing} ->
          ?INFO("Publish failed", []),
          {close, 4001, <<>>}
      end;

    _ ->
      {close, 4002, <<>>}
  end;

handle_message(<<"rtc">>,
               Payload,
               InnerState = #connected{ session_id = SessionId
                                      , webrtc_session_ref = InitialRTCSessionRef
                                      , stream_details = #{ streamDetails := #{ slot := #{ id := SlotId }
                                                                              , role := SlotRole
                                                                              }
                                                          , profileName := ProfileName
                                                          }
                                      },
               #state{ me = Me
                     , account = Account
                     , stream_name = StreamName
                     }
              ) ->

  {RTCSessionRef, Response} = try
                                {InitialRTCSessionRef, webrtc_session:handle_request(SessionId, Payload)}
                              catch
                                exit:{noproc, _} ->
                                  ?SLOG_NOTICE("Starting WebRTC Session", #{ account => Account
                                                                           , stream_name => StreamName}),
                                  {ok, Pid} = webrtc_session:start_link(SessionId,
                                                                        Me,
                                                                        rtsv2_webrtc_push_ingest_handler,
                                                                        [ SlotId, SlotRole, ProfileName ]
                                                                       ),

                                  %% Make sure we go away if/when the RTC session
                                  %% goes down
                                  NewRTCSessionRef = monitor(process, Pid),

                                  {NewRTCSessionRef, webrtc_session:handle_request(SessionId, Payload)}
                              end,

  NewInnerState = InnerState#connected{ webrtc_session_ref = RTCSessionRef },

  case Response of
    {ok, #webrtc_session_response{payload = ResponsePayload}} ->
      {reply, make_session_response(ResponsePayload), NewInnerState};

    ok ->
      {ok, NewInnerState};

    {error, unsupported} ->
      %% future client? in any event, webrtc_session will have logged something
      {ok, NewInnerState}
  end;

handle_message(<<"ping">>, Data, InnerState = #connected{}, _OuterState) ->
  {reply, make_response(<<"pong">>, Data), InnerState}.

websocket_info(#webrtc_session_response { payload = Payload }, State) ->
  {reply, {text, jsx:encode(make_session_response(Payload))}, State};

websocket_info({'DOWN', Ref, process, _Pid, _Reason}, State = #connected{webrtc_session_ref = RTCSessionRef})
  when
    RTCSessionRef =:= Ref
    ->

  {ok, State};

websocket_info(_Info, State) ->
  ?INFO("HERE WITH ~p", [_Info]),
  {ok, State}.

make_session_response(Payload) ->
  make_response(rtc, Payload).

make_response(Type, undefined) ->
  [ {type, Type}
  ];


make_response(Type, Payload) ->
  [ {type, Type}
  , {data, Payload}
  ].

update_state(State, NewInnerState) ->
  State#state{ state = NewInnerState
             , last_activity = i_timestamp:utc_now()
             }.

%% maybe_disconnect_existing(_SessionId = undefined) ->
%%   ok;
%% maybe_disconnect_existing(_SessionId = null) ->
%%   ok;
%% maybe_disconnect_existing(SessionId) ->
%%   webrtc_session:stop(SessionId).

%% maybe_generate_session_id(_SessionId = undefined) ->
%%   generate_session_id();
%% maybe_generate_session_id(_SessionId = null) ->
%%   generate_session_id();
%% maybe_generate_session_id(SessionId) ->
%%   SessionId.

generate_session_id() ->
  uuid:uuid_to_string(uuid:get_v4(), binary_standard).
