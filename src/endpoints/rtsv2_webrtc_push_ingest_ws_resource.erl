-module(rtsv2_webrtc_push_ingest_ws_resource).

-include_lib("kernel/include/inet.hrl").

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

-define(state_initializing,  rtsv2_websrc_push_ingest_ws_resource_state_initializing).
-define(state_authenticated, rtsv2_websrc_push_ingest_ws_resource_state_authenticated).
-define(state_ingesting,     rtsv2_websrc_push_ingest_ws_resource_state_ingesting).
-define(state_failed,        rtsv2_websrc_push_ingest_ws_resource_state_failed).

-record(stream_desc,
        { remote_host :: binary_string()
        , remote_port :: non_neg_integer()
        , account :: binary_string()
        , stream_name :: binary_string()
        , authenticate :: fun()
        , publish_stream :: fun()
        , make_ingest_key :: fun()
        , stop_stream :: fun()
        }).

-record(?state_initializing,
        { stream_desc :: #stream_desc{}
        , trace_id :: binary()
        , public_ip_string :: binary_string()
        , ice_servers :: list(map())
        }).

-record(?state_authenticated,
        { stream_desc :: #stream_desc{}
        , trace_id :: binary()
        , public_ip_string :: binary_string()
        , username :: binary()
        }).

-record(?state_ingesting,
        { stream_desc :: #stream_desc{}
        , trace_id :: binary()
        , public_ip_string :: binary_string()
        , webrtc_session_pid :: pid()
        , webrtc_session_ref :: reference()
        , workflow_pid :: pid()
        , stream_details :: term()
        , profile_name :: term()
        , username :: binary()
        , source_info_fun :: fun()
        }).

-define(WebSocketStatusCode_DataInconsistentWithType, 1007).
-define(WebSocketStatusCode_MessageBad, 4001).
-define(WebSocketStatusCode_StateBad, 4002).
-define(WebSocketStatusCode_InvalidSDP, 4003).
-define(WebSocketStatusCode_AuthenticationFailed, 4004).

init(Req, #{ authenticate := Authenticate
           , publish_stream := PublishStream
           , make_ingest_key := MakeIngestKey
           , stop_stream := StopStream }) ->

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
                           , make_ingest_key = MakeIngestKey
                           , publish_stream = PublishStream
                           , stop_stream = StopStream
                           },

  {cowboy_websocket
  , Req
  , #?state_initializing{ stream_desc = StreamDesc
                        , trace_id = TraceId
                        , ice_servers = IceServers
                        , public_ip_string = PublicIPString
                        }
  }.

terminate(_Reason, _PartialReq, #?state_ingesting{ webrtc_session_pid = SessionPid
                                                 , workflow_pid = WorkflowPid }) ->
  gen_server:stop(SessionPid),
  id3as_workflow:stop(WorkflowPid),
  ok;

terminate(_Reason, _PartialReq, _State) ->
  ok.

websocket_init(State = #?state_initializing{ trace_id = TraceId }) ->
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
          handle_ice_gathering_done(Message, State)

        %% TODO
        %% <<"dataobject.send-message">> ->
        %%   handle_data_object_send_message(Message, State);

        %% <<"dataobject.update">> ->
        %%   handle_data_object_update(Message, State);


        %% <<"rtc">> ->
        %%   handle_rtc(Message, State)

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

websocket_info(send_fir, State = #?state_ingesting{trace_id = TraceId}) ->
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
      };

    Other ->
      ?INFO("HERE ~p", [Other]),
      { []
      , State
      }
  end;

websocket_info({'DOWN', Ref, process, _Pid, _Reason}, State = #?state_ingesting{webrtc_session_ref = RTCSessionRef})
  when RTCSessionRef =:= Ref ->
  { [ close_frame(1001) ]
  , State
  };

websocket_info(#workflow_output{message = #workflow_data_msg{data = SourceInfo = #source_info{}}}, State = #?state_ingesting { source_info_fun = SourceInfoFn }) ->
  unit = (SourceInfoFn(SourceInfo))(),
  {ok, State};

websocket_info(_Info, State) ->
  ?INFO("HERE WITH ~p", [_Info]),
  {ok, State}.

handle_authenticate(#{ <<"username">> := Username
                     , <<"password">> := Password }, State = #?state_initializing{ trace_id = TraceId
                                                                                 , ice_servers = IceServers
                                                                                 , public_ip_string = PublicIpString
                                                                                 , stream_desc = StreamDesc = #stream_desc{ account = Account
                                                                                                                          , authenticate = Authenticate
                                                                                                                          }
                                                                                 }) ->

  case (Authenticate(Account, Username, Password))() of

    {just, unit } ->

      { [ json_frame( <<"authenticated">>,
                      #{ thisIngest =>
                           #{ iceServers => IceServers
                            }
                       }
                    ) ],
        #?state_authenticated{ trace_id = TraceId
                             , stream_desc = StreamDesc
                             , public_ip_string = PublicIpString
                             , username = Username
                             }
      };

    {nothing} ->
      { [ close_frame(?WebSocketStatusCode_AuthenticationFailed) ]
      , State
      }
  end;

handle_authenticate(#{ <<"username">> := _
                     , <<"password">> := _ }, State) when not is_record(State, ?state_initializing) ->
  { [ close_frame(?WebSocketStatusCode_StateBad) ]
  , State
  };

handle_authenticate(_Data, State) ->
  { [ close_frame(?WebSocketStatusCode_MessageBad) ]
  , State
  }.

handle_start_ingest(#{ }, State = #?state_authenticated{ trace_id = TraceId
                                                       , username = Username
                                                       , public_ip_string = PublicIpString
                                                       , stream_desc = StreamDesc = #stream_desc{ account = Account
                                                                                                , stream_name = StreamName
                                                                                                , remote_host = RemoteHost
                                                                                                , remote_port = RemotePort
                                                                                                , publish_stream = PublishStream
                                                                                                }
                                                       }) ->

  case (PublishStream(Account, Username, RemoteHost, RemotePort, StreamName))() of

    {just, #{ streamDetails := StreamDetails = #{ slot := #{ id := SlotId }
                                                , role := SlotRole
                                                }
            , profileName := ProfileName
            , sourceInfo := SourceInfoFn
            , workflowPid := WorkflowPid}} ->

      {ok, Pid} = webrtc_session:start_link(TraceId,
                                            PublicIpString,
                                            rtsv2_webrtc_push_ingest_handler,
                                            [ SlotId, SlotRole, ProfileName ]
                                           ),
      webrtc_session:subscribe_for_msgs(TraceId, [#webrtc_session_response{}]),

      %% Make sure we go away if/when the RTC session
      %% goes down
      NewRTCSessionRef = monitor(process, Pid),

      { [ json_frame( <<"ingest-started">>,
                      #{}
                    ) ],
        #?state_ingesting{ stream_desc = StreamDesc
                         , trace_id = TraceId
                         , public_ip_string = PublicIpString
                         , stream_details = StreamDetails
                         , profile_name = ProfileName
                         , source_info_fun = SourceInfoFn
                         , username = Username
                         , webrtc_session_pid = Pid
                         , workflow_pid = WorkflowPid
                         , webrtc_session_ref = NewRTCSessionRef
                         }
      };

    {nothing} ->
      { [ close_frame(?WebSocketStatusCode_AuthenticationFailed) ]
      , State
      }
  end;

handle_start_ingest(_, State) ->
  { [ close_frame(?WebSocketStatusCode_StateBad) ]
  , State
  }.

handle_stop_ingest(_, _State = #?state_ingesting{ webrtc_session_pid = SessionPid
                                                , workflow_pid = WorkflowPid
                                                , stream_desc = StreamDesc = #stream_desc{ stop_stream = StopStream
                                                                                         , make_ingest_key = MakeIngestKey
                                                                                         }
                                                , stream_details = StreamDetails
                                                , profile_name = ProfileName
                                                , trace_id = TraceId
                                                , public_ip_string = PublicIpString
                                                , username = Username}) ->
  gen_server:stop(SessionPid),
  id3as_workflow:stop(WorkflowPid),
  (StopStream(MakeIngestKey(StreamDetails, ProfileName)))(),

  { [ json_frame( <<"ingest-stopped">>,
                  #{}
                ) ],
    #?state_authenticated{ stream_desc = StreamDesc
                         , trace_id = TraceId
                         , public_ip_string = PublicIpString
                         , username = Username
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

handle_sdp_offer(#{ <<"offer">> := SDP }, #?state_ingesting{ trace_id = TraceId } = State) ->
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

handle_ice_gathering_candidate(#{ <<"candidate">> := Candidate, <<"index">> := MediaLineIndex }, #?state_ingesting{ trace_id = TraceId } = State) ->
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

generate_trace_id() ->
  uuid:uuid_to_string(uuid:get_v4(), binary_standard).

json_frame(Type) ->  json_frame(Type, undefined).

json_frame(Type, undefined) -> json_frame(Type, #{});

json_frame(Type, Data) -> {text, jsx:encode(Data#{ <<"type">> => Type})}.

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
