-module(rtsv2_webrtc_push_ingest_ws_resource).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_rtc/include/webrtc.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_avp/include/avp_ingest_source_details_extractor.hrl").

-include("../rtsv2_types.hrl").

-export([ init/2
        , terminate/3]).

-export([ websocket_init/1
        , websocket_handle/2
        , websocket_info/2
        ]).

-record(pending, {}).

-record(connected,
        { session_id :: binary()
        , webrtc_session_ref :: reference()
        , stream_details :: term()
        , profile_name :: term()
        , source_info_fun :: fun()
        }).

-record(state,
        { me :: binary_string()
        , remote_host :: binary_string()
        , remote_port :: non_neg_integer()
        , account :: binary_string()
        , stream_name :: binary_string()
        , state = #pending{} :: #pending{} | #connected{}
        , publish_stream :: fun()
        }).

-define(WebSocketStatusCode_DataInconsistentWithType, 1007).
-define(WebSocketStatusCode_MessageBad, 4001).
-define(WebSocketStatusCode_StateBad, 4002).
-define(WebSocketStatusCode_AuthenticationFailed, 4003).

init(Req, #{ publish_stream := PublishStream }) ->

  RemoteHost = cowboy_req:host(Req),
  RemotePort = cowboy_req:port(Req),
  Account = cowboy_req:binding(account, Req),
  StreamName = cowboy_req:binding(stream_name, Req),

  Me = cowboy_req:host(Req),

  timer:send_interval(1000, send_fir),

  {cowboy_websocket
  , Req
  , #state{ me = Me
          , remote_host = RemoteHost
          , remote_port = RemotePort
          , account = Account
          , stream_name = StreamName
          , publish_stream = PublishStream
          }
  }.

terminate(_Reason, _PartialReq, _State) ->
  ok.

websocket_init(State) ->
  timer:send_interval(1000, send_fir),

  {ok, State}.

websocket_handle({text, JSON}, State) ->

  try
    jsx:decode(JSON, [return_maps])
  of
    #{ <<"type">> := Type } = Message ->

      case Type of
        <<"ping">> ->
          handle_ping(State);

        <<"join">> ->
          handle_join(Message, State);

        <<"rtc">> ->
          handle_rtc(Message, State)

      end;

    _BadlyFormattedMessage ->
      { [ close_frame(?WebSocketStatusCode_DataInconsistentWithType) ]
      , State
      }

  catch
    error:badarg ->
      { [ close_frame(?WebSocketStatusCode_DataInconsistentWithType) ]
      , State
      }
  end.

websocket_info(send_fir, State = #state{ state = #pending{} }) ->
  {ok, State};

websocket_info(send_fir, State = #state{ state = #connected{session_id = SessionId} }) ->
  webrtc_session:send_fir(SessionId),
  {ok, State};

websocket_info(#webrtc_session_response { payload = Payload }, State) ->
  { [ json_frame( <<"rtc">>,
                  #{payload => Payload} ) ],
    State
  };

websocket_info({'DOWN', Ref, process, _Pid, _Reason}, State = #connected{webrtc_session_ref = RTCSessionRef})
  when
    RTCSessionRef =:= Ref
    ->

  {ok, State};

websocket_info(#workflow_output{message = #workflow_data_msg{data = SourceInfo = #source_info{}}}, State = #state { state = #connected { source_info_fun = SourceInfoFn } }) ->
  unit = (SourceInfoFn(SourceInfo))(),
  {ok, State};

websocket_info(_Info, State) ->
  ?INFO("HERE WITH ~p", [_Info]),
  {ok, State}.

handle_join(#{ <<"username">> := Username
             , <<"password">> := Password }, State = #state{ account = Account
                                                           , stream_name = StreamName
                                                           , remote_host = RemoteHost
                                                           , remote_port = RemotePort
                                                           , publish_stream = PublishStream
                                                           , state = #pending{}
                                                           }) ->

  case (PublishStream(Account, Username, Password, RemoteHost, RemotePort, StreamName))() of

    {just, #{ streamDetails := StreamDetails
            , profileName := ProfileName
            , sourceInfo := SourceInfoFn}} ->
      SessionId = generate_session_id(),
      webrtc_session:subscribe_for_msgs(SessionId, [#webrtc_session_response{}]),

      { [ json_frame( <<"join">>,
                      #{session_id => SessionId}
                    ) ],
        State#state{state = #connected{ session_id = SessionId
                                      , stream_details = StreamDetails
                                      , profile_name = ProfileName
                                      , source_info_fun = SourceInfoFn
                                      }}
      };

    {nothing} ->
      { [ close_frame(?WebSocketStatusCode_AuthenticationFailed) ]
      , State
      }
  end;

handle_join(#{ <<"username">> := _
             , <<"password">> := _ }, State = #state{ state = #connected{} }) ->
  { [ close_frame(?WebSocketStatusCode_StateBad) ]
  , State
  };

handle_join(_Data, State) ->
  { [ close_frame(?WebSocketStatusCode_MessageBad) ]
  , State
  }.

handle_rtc(#{ <<"data">> := Payload },
           OuterState = #state{ me = Me
                              , account = Account
                              , stream_name = StreamName
                              , state = InnerState = #connected{ session_id = SessionId
                                                               , webrtc_session_ref = InitialRTCSessionRef
                                                               , stream_details = #{ slot := #{ id := SlotId }
                                                                                   , role := SlotRole
                                                                                   }
                                                               , profile_name = ProfileName
                                                               }
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

  NewOuterState = OuterState#state{state = NewInnerState},

  case Response of
    {ok, #webrtc_session_response{payload = ResponsePayload}} ->
      { [ json_frame( <<"rtc">>,
                      #{payload => ResponsePayload} ) ],
        NewOuterState
      };

    ok ->
      {[], NewOuterState };

    {error, unsupported} ->
      %% future client? in any event, webrtc_session will have logged something
      {[], NewOuterState }
  end;

handle_rtc(#{ <<"data">> := _}, State = #state { state = #pending{} }) ->
  { [ close_frame(?WebSocketStatusCode_StateBad) ]
  , State
  };

handle_rtc(_Data, State) ->
  { [ close_frame(?WebSocketStatusCode_MessageBad) ]
  , State
  }.

handle_ping(State) ->
  { [ json_frame(<<"pong">>) ]
  , State
  }.

generate_session_id() ->
  uuid:uuid_to_string(uuid:get_v4(), binary_standard).

json_frame(Type) ->  json_frame(Type, undefined).

json_frame(Type, undefined) -> json_frame(Type, #{});

json_frame(Type, Data) -> {text, jsx:encode(Data#{ <<"type">> => Type})}.

close_frame(Code) ->
  close_frame(Code, <<>>).

close_frame(Code, Reason) ->
  {close, Code, Reason}.
