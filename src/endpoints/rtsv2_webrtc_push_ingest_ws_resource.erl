-module(rtsv2_webrtc_push_ingest_ws_resource).

-include_lib("kernel/include/inet.hrl").

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_common/include/id3as_message_bus.hrl").
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

-define(state_initializing,  rtsv2_websrc_push_ingest_ws_resource_state_initializing).
-define(state_authenticated, rtsv2_websrc_push_ingest_ws_resource_state_authenticated).
-define(state_ingesting,     rtsv2_websrc_push_ingest_ws_resource_state_ingesting).
-define(state_failed,        rtsv2_websrc_push_ingest_ws_resource_state_failed).

-record(stream_desc,
        { trace_id :: binary()
        , public_ip_string :: binary_string()
        , remote_host :: binary_string()
        , remote_port :: non_neg_integer()
        , account :: binary_string()
        , stream_name :: binary_string()
        , authenticate :: fun()
        }).

-record(authentication_response,
        { start_stream :: fun()
        , data_object_send_message :: fun()
        , data_object_update :: fun()
        , stream_details :: term()
        , protocol :: {webRTC} | {rtmp}
        , slot_id :: slot_id()
        , slot_role :: slot_role()
        , profile_name :: profile_name()
        , username :: binary()
        }).

-record(?state_initializing,
        { stream_desc :: #stream_desc{}
        , ice_servers :: list(map())
        }).

-record(?state_authenticated,
        { stream_desc :: #stream_desc{}
        , authentication_response :: #authentication_response{}
        }).

-record(?state_ingesting,
        { stream_desc :: #stream_desc{}
        , authentication_response :: #authentication_response{}
        , webrtc_session_pid :: pid()
        , webrtc_session_ref :: reference()
        , workflow_pid :: pid()
        , stop_stream :: fun()
        , source_info :: fun()
        }).

-define(WebSocketStatusCode_WebRTCSessionFailed, {1001, <<"WebRTC session failed">>}).
-define(WebSocketStatusCode_DataInconsistentWithType, {1007, <<"Malformed message">>}).
-define(WebSocketStatusCode_MessageBad, {4001, <<"Invalid data in RTP negotiation">>}).
-define(WebSocketStatusCode_StateBad, {4002, <<"Request made whilst in invalid state">>}).
-define(WebSocketStatusCode_InvalidSDP, {4003, <<"Invalid SDP">>}).
-define(WebSocketStatusCode_AuthenticationFailed, {4004, <<"Authentication failed">>}).
-define(WebSocketStatusCode_IngestAlreadyRunning, {4005, <<"Ingest already active">>}).

init(Req, #{ authenticate := Authenticate }) ->

  PublicIP = this_server_ip(Req),
  PublicIPString = i_convert:convert(PublicIP, binary_string),
  TurnIP = PublicIP,
  TurnPort = 4000,
  TurnUsername = <<"username">>,
  TurnPassword = <<"password">>,

  RemoteHost = cowboy_req:host(Req),
  RemotePort = cowboy_req:port(Req),
  Account = cowboy_req:binding(account, Req),
  StreamName = cowboy_req:binding(stream_name, Req),
  TraceId = generate_trace_id(),
  IceServers = stun_turn_config(none, TurnIP, TurnPort, TurnUsername, TurnPassword),

  StreamDesc = #stream_desc{ remote_host = RemoteHost
                           , remote_port = RemotePort
                           , account = Account
                           , stream_name = StreamName
                           , authenticate = Authenticate
                           , trace_id = TraceId
                           , public_ip_string = PublicIPString
                           },

  {cowboy_websocket
  , Req
  , #?state_initializing{ stream_desc = StreamDesc
                        , ice_servers = IceServers
                        }
  }.

terminate(_Reason, _PartialReq, #?state_ingesting{ webrtc_session_pid = SessionPid
                                                 , workflow_pid = WorkflowPid }) ->
  gen_server:stop(SessionPid),
  id3as_workflow:stop(WorkflowPid),
  ok;

terminate(_Reason, _PartialReq, _State) ->
  ok.

websocket_init(State = #?state_initializing{ stream_desc = #stream_desc{ trace_id = TraceId }}) ->
  timer:send_interval(1000, send_fir),

  %% Needs to come from config!

  InitialMessage = #{ traceId => TraceId },

  { [ json_frame(<<"init">>, InitialMessage) ]
  , State
  }.

websocket_handle({text, JSON}, State) ->

  try
    jsx:decode(JSON, [return_maps])
  of
    #{ <<"type">> := Type } = Message ->

      case Type of
        <<"authenticate">> ->
          handle_authenticate(Message, State);

        <<"ping">> ->
          handle_ping(State);

        <<"start-ingest">> ->
          handle_start_ingest(Message, State);

        <<"stop-ingest">> ->
          handle_stop_ingest(Message, State);

        <<"sdp.offer">> ->
          handle_sdp_offer(Message, State);

        <<"ice.candidate">> ->
          handle_ice_gathering_candidate(Message, State);

        <<"ice.done">> ->
          handle_ice_gathering_done(Message, State);

        <<"dataobject.send-message">> ->
          handle_data_object_send_message(Message, State);

        <<"dataobject.update">> ->
          handle_data_object_update(Message, State)
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

websocket_info(send_fir, State = #?state_ingesting{stream_desc = #stream_desc{trace_id = TraceId}}) ->
  webrtc_session:send_fir(TraceId),
  {ok, State};

websocket_info(send_fir, State) ->
  {ok, State};

websocket_info(#webrtc_session_response{payload = Payload}, State) ->

  case jsx:decode(Payload, [return_maps]) of
    #{ <<"server_ice_candidate">> :=
         #{ <<"sdpMLineIndex">> := MediaLineIndex
          , <<"candidate">> := Candidate
          }
     } ->
      { [ json_frame( <<"ice.candidate">>,
                      #{ <<"index">> => MediaLineIndex
                       , <<"candidate">> => Candidate
                       }
                    ) ]
      , State
      }
  end;

websocket_info({ingestDataObjectMessage, #{ msg := Msg
                                          , sender := Sender }}, State) when is_record(State, ?state_authenticated);
                                                                             is_record(State, ?state_ingesting) ->

  {[ json_frame( <<"dataobject.message">>,
                 #{ <<"sender">> => Sender
                  , <<"msg" >> => Msg
                  }
               ) ]
  , State};

websocket_info({ingestDataObjectUpdateResponse,#{response := Response
                                                , senderRef := SenderRef
                                                , to := To}}, State) when is_record(State, ?state_authenticated);
                                                                          is_record(State, ?state_ingesting) ->

  TraceId = case State of
              #?state_authenticated{ stream_desc = #stream_desc{trace_id = Id}} -> Id;
              #?state_ingesting{ stream_desc = #stream_desc{trace_id = Id}} -> Id
            end,

  if
    To == TraceId ->
      { [ json_frame( <<"dataobject.update-response">>,
                      #{ <<"senderRef">> => SenderRef,
                         <<"response">> => endpoint_helpers:dataobject_response_to_ts(Response)
                       }
                    ) ]
        , State
      };
    ?otherwise ->
      {ok, State}
  end;

websocket_info({ingestDataObjectBroadcast, Object}, State) when is_record(State, ?state_authenticated);
                                                                is_record(State, ?state_ingesting) ->
  { [ json_frame( <<"dataobject.broadcast">>,
                  #{ <<"object">> => endpoint_helpers:dataobject_to_ts(Object)
                   }
                ) ]
  , State
  };

websocket_info(#workflow_output{message = #workflow_data_msg{data = SourceInfo = #source_info{}}}, State = #?state_ingesting { source_info = SourceInfoFn }) ->
  unit = (SourceInfoFn(SourceInfo))(),
  {ok, State};

websocket_info({'DOWN', Ref, process, _Pid, Reason}, State = #?state_ingesting{webrtc_session_ref = RTCSessionRef})
  when RTCSessionRef =:= Ref ->
  ?SLOG_INFO("WebRTC Session down", #{reason => Reason}),
  { [ close_frame(?WebSocketStatusCode_WebRTCSessionFailed) ]
  , State
  };

websocket_info(_Info, State) ->
  ?INFO("HERE WITH ~p", [_Info]),
  {ok, State}.

handle_authenticate(#{ <<"username">> := Username
                     , <<"password">> := Password
                     , <<"protocol">> := Protocol }, State = #?state_initializing{ ice_servers = IceServers
                                                                                 , stream_desc = StreamDesc = #stream_desc{ account = Account
                                                                                                                          , stream_name = StreamName
                                                                                                                          , remote_host = RemoteHost
                                                                                                                          , remote_port = RemotePort
                                                                                                                          , authenticate = Authenticate
                                                                                                                          }
                                                                                 }) when Protocol == <<"webrtc">>;
                                                                                         Protocol == <<"rtmp">> ->

  PursProtocol = case Protocol of
                   <<"webrtc">> -> {webRTC};
                   <<"rtmp">> -> {rtmp}
                 end,

  case (Authenticate(PursProtocol, Account, Username, Password, StreamName, RemoteHost, RemotePort))() of

    {just, #{ streamDetails := #{ slot := #{ id := SlotId }
                                , role := SlotRole
                                }
            , profileName := ProfileName
            , startStream := StartStream
            , dataObjectSendMessage := SendMessage
            , dataObjectUpdate := ObjectUpdate
            }} ->

      ?I_SUBSCRIBE_BUS_MSGS({ingestBus, {ingestKey, SlotId, SlotRole, ProfileName}}),

      { [ json_frame( <<"authenticated">>,
                      #{ thisIngest =>
                           #{ iceServers => IceServers
                            }
                       }
                    ) ],
        #?state_authenticated{ stream_desc = StreamDesc
                             , authentication_response = #authentication_response{ username = Username
                                                                                 , slot_id = SlotId
                                                                                 , slot_role = SlotRole
                                                                                 , profile_name = ProfileName
                                                                                 , protocol = PursProtocol
                                                                                 , start_stream = StartStream
                                                                                 , data_object_send_message = SendMessage
                                                                                 , data_object_update = ObjectUpdate
                                                                                 }
                             }
      };

    {nothing} ->
      { [ close_frame(?WebSocketStatusCode_AuthenticationFailed) ]
      , State
      }
  end;

handle_authenticate(#{ <<"username">> := _
                     , <<"password">> := _
                     , <<"protocol">> := Protocol}, State) when not is_record(State, ?state_initializing),
                                                                (Protocol == <<"webrtc">> orelse Protocol == <<"rtmp">>) ->
  { [ close_frame(?WebSocketStatusCode_StateBad) ]
  , State
  };

handle_authenticate(_Data, State) ->
  { [ close_frame(?WebSocketStatusCode_DataInconsistentWithType) ]
  , State
  }.

handle_start_ingest(#{ }, State = #?state_authenticated{ stream_desc =
                                                           StreamDesc = #stream_desc{ trace_id = TraceId
                                                                                    , public_ip_string = PublicIPString
                                                                                    }
                                                       , authentication_response =
                                                           AuthenticationResponse = #authentication_response{ slot_id = SlotId
                                                                                                            , slot_role = SlotRole
                                                                                                            , profile_name = ProfileName
                                                                                                            , start_stream = {just, StartStreamFn}
                                                                                                            , protocol = {webRTC}
                                                                                                            }
                                                       }) ->

  case StartStreamFn() of

    {just, #{ sourceInfo := SourceInfoFn
            , stopStream := StopStreamFn
            , workflowPid := WorkflowPid}} ->

      {ok, Pid} = webrtc_session:start_link(TraceId,
                                            PublicIPString,
                                            rtsv2_webrtc_push_ingest_handler,
                                            [ SlotId, SlotRole, ProfileName ]
                                           ),
      webrtc_session:subscribe_for_msgs(TraceId, [#webrtc_session_response{}]),

      %% Make sure we go away if/when the RTC session goes down
      NewRTCSessionRef = monitor(process, Pid),

      { [ json_frame( <<"ingest-started">>,
                      #{}
                    ) ],
        #?state_ingesting{ stream_desc = StreamDesc
                         , authentication_response = AuthenticationResponse
                         , source_info = SourceInfoFn
                         , stop_stream = StopStreamFn
                         , webrtc_session_pid = Pid
                         , workflow_pid = WorkflowPid
                         , webrtc_session_ref = NewRTCSessionRef
                         }
      };

    {nothing} ->
      { [ close_frame(?WebSocketStatusCode_IngestAlreadyRunning) ]
      , State
      }
  end;

handle_start_ingest(_, State) ->
  { [ close_frame(?WebSocketStatusCode_StateBad) ]
  , State
  }.

handle_stop_ingest(_, _State = #?state_ingesting{ webrtc_session_pid = SessionPid
                                                , workflow_pid = WorkflowPid
                                                , stop_stream = StopStreamFn
                                                , stream_desc = StreamDesc
                                                , authentication_response = AuthenticationResponse
                                                }) ->
  gen_server:stop(SessionPid),
  id3as_workflow:stop(WorkflowPid),
  StopStreamFn(),

  { [ json_frame( <<"ingest-stopped">>,
                  #{}
                ) ],
    #?state_authenticated{ stream_desc = StreamDesc
                         , authentication_response = AuthenticationResponse
                         }
  };

handle_stop_ingest(_, State) ->
  { [ close_frame(?WebSocketStatusCode_StateBad) ]
  , State
  }.

handle_ping(State) ->
  { [ json_frame(<<"pong">>) ]
  , State
  }.

handle_sdp_offer(#{ <<"offer">> := SDP },
                 #?state_ingesting{ stream_desc = #stream_desc{ trace_id = TraceId }} = State) ->

  case webrtc_session:handle_client_offer(TraceId, SDP) of
    {ok, ResponseSDP} ->
      ?LOG_DEBUG(#{ what => "negotiation.offer-received", result => "ok", context => #{ offer => SDP, response => ResponseSDP } }),
      { [ json_frame( <<"sdp.offer-response">>, #{ <<"response">> => ResponseSDP } ) ]
      , State
      };

    {error, Reason} ->
      ?LOG_DEBUG(#{ what => "negotiation.offer-received", result => "error", offer => SDP, reason => Reason }),
      { [ close_frame(?WebSocketStatusCode_InvalidSDP) ]
      , State
      }
  end;

handle_sdp_offer(BadMessage, State) ->
  ?LOG_DEBUG(#{ what => "negotiation.offer-received", result => "error", reason => "offer field missing", bad_message => BadMessage }),
  { [ close_frame(?WebSocketStatusCode_MessageBad) ]
  , State
  }.

handle_ice_gathering_candidate(#{ <<"candidate">> := Candidate
                                , <<"index">> := MediaLineIndex },
                               #?state_ingesting{ stream_desc = #stream_desc{ trace_id = TraceId }} = State) ->

  case webrtc_session:handle_ice_candidate(TraceId, MediaLineIndex, Candidate) of
    ok ->
      ?LOG_DEBUG(#{ what => "negotiation.ice-candidate-received", result => "ok", candidate => Candidate, media_line_index => MediaLineIndex });

    {error, Reason} ->
      ?LOG_DEBUG(#{ what => "negotiation.ice-candidate-received", result => "error", reason => Reason, candidate => Candidate, media_line_index => MediaLineIndex })
  end,

  { [], State };

handle_ice_gathering_candidate(BadMessage, State) ->

  ?LOG_DEBUG(#{ what => "negotiation.ice-candidate-received", result => "error", reason => "one or more fields missing", bad_message => BadMessage }),
  { [ close_frame(?WebSocketStatusCode_MessageBad) ]
  , State
  }.

handle_ice_gathering_done(_Message, State) ->

  ?LOG_DEBUG(#{ what => "negotiation.ice-gathering-done", result => "ok" }),
  { []
  , State
  , hibernate
  }.

handle_data_object_send_message(#{ <<"msg">> := Message
                                 , <<"destination">> := Destination },
                                State) when is_record(State, ?state_authenticated);
                                            is_record(State, ?state_ingesting) ->

  {SendMessageFn, TraceId} = case State of
                               #?state_authenticated{ stream_desc = #stream_desc{trace_id = Id}
                                                    , authentication_response = #authentication_response{data_object_send_message = Fn}} -> {Fn, Id};
                               #?state_ingesting{ stream_desc = #stream_desc{trace_id = Id}
                                                , authentication_response = #authentication_response{data_object_send_message = Fn}} -> {Fn, Id}
                             end,

  PursMessageDestination = case Destination of
                             #{ <<"tag">> := <<"publisher">> } -> {publisher};
                             #{ <<"tag">> := <<"broadcast">> } -> {broadcast};
                             #{ <<"tag">> := <<"private">>, <<"to">> :=  To } -> {private, To}
                           end,

  PursMessage = #{ sender => TraceId
                 , destination => PursMessageDestination
                 , msg => Message
                 , ref => make_ref()
                 },

  (SendMessageFn(PursMessage))(),

  {ok, State}.

handle_data_object_update(#{ <<"operation">> := Operation,
                             <<"senderRef">> := SenderRef },
                          State) when is_record(State, ?state_authenticated);
                                      is_record(State, ?state_ingesting) ->

  {UpdateFn, TraceId} = case State of
                          #?state_authenticated{ stream_desc = #stream_desc{trace_id = Id}
                                               , authentication_response = #authentication_response{data_object_update = Fn}} -> {Fn, Id};
                          #?state_ingesting{ stream_desc = #stream_desc{trace_id = Id}
                                           , authentication_response = #authentication_response{data_object_update = Fn}} -> {Fn, Id}
                        end,

  try
    PursOperation = endpoint_helpers:dataobject_operation_to_purs(Operation),
    PursMessage = #{ sender => TraceId
                   , senderRef => SenderRef
                   , operation => PursOperation
                   , ref => make_ref()
                   },

    (UpdateFn(PursMessage))(),

    {ok, State}
  catch
    _Class:_Reason ->
      io:format(user, "Invalid Request: ~p: ~p~n", [Operation, {_Class, _Reason}]),
      { [ json_frame( <<"dataobject.update-response">>,
                      #{ <<"senderRef">> => SenderRef,
                         <<"response">> => <<"invalidRequest">>} ) ]
      , State
      }
  end;

handle_data_object_update(Request = #{ <<"senderRef">> := SenderRef }, State) ->
  io:format(user, "Invalid Request: ~p~n", [Request]),
  { [ json_frame( <<"dataobject.update-response">>,
                  #{ <<"senderRef">> => SenderRef,
                     <<"response">> => <<"invalidRequest">>} ) ]
  , State
  };

handle_data_object_update(Request, State) ->
  io:format(user, "Invalid Request: ~p~n", [Request]),
  { [ json_frame( <<"dataobject.update-response">>,
                  #{ <<"senderRef">> => <<"unknown">>,
                     <<"response">> => <<"invalidRequest">>} ) ]
  , State
  }.

generate_trace_id() ->
  uuid:uuid_to_string(uuid:get_v4(), binary_standard).

json_frame(Type) ->  json_frame(Type, undefined).

json_frame(Type, undefined) -> json_frame(Type, #{});

json_frame(Type, Data) -> {text, jsx:encode(Data#{ <<"type">> => Type})}.

close_frame({Code, Reason}) ->
  close_frame(Code, Reason);

close_frame(Code) ->
  close_frame(Code, <<>>).

close_frame(Code, Reason) ->
  {close, Code, Reason}.

stun_turn_config(local_turn, IP, Port, Username, Password) ->
  [ local_stun_server(IP, Port)
  , local_turn_server(IP, Port, Username, Password)
  ];

stun_turn_config(google_stun, _IP, _Port, _Username, _Password) ->
  [ google_stun_server()
  ];

stun_turn_config(none, _IP, _Port, _Username, _Password) ->
  [
  ].


google_stun_server() ->
  #{ urls => [ <<"stun:stun1.l.google.com:19302">> ] }.


local_stun_server(IP, Port) ->
  #{ urls => [ <<"stun:", IP/binary, ":", (integer_to_binary(Port))/binary >> ] }.


local_turn_server(IP, Port, Username, Password) ->
  #{ urls => [ <<"turn:", IP/binary, ":", (integer_to_binary(Port))/binary >> ]
   , username => Username
   , credential => Password
   }.

this_server_ip(Req) ->
  Host = cowboy_req:host(Req),
  {ok, #hostent{h_addr_list = AddrList}} = inet:gethostbyname(binary_to_list(Host)),
  IP = i_convert:convert(hd(AddrList), binary_string),
  IP.
