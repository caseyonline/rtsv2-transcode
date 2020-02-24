-module(rtsv2_player_ws_resource).


-include_lib("kernel/include/inet.hrl").
-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_common/include/id3as_message_bus.hrl").
-include_lib("id3as_rtc/include/webrtc.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include("./rtsv2_rtp.hrl").


-export([ init/2
        , terminate/3
        ]).


-export([ websocket_init/1
        , websocket_handle/2
        , websocket_info/2
        ]).


-define(state_running,      rtsv2_player_ws_resource_state_running).
-define(state_redirect,     rtsv2_player_ws_resource_state_redirect).
-define(state_not_found,    rtsv2_player_ws_resource_state_not_found).
-define(state_retry,        rtsv2_player_ws_resource_state_retry).

-define(stream_desc_ingest, rtsv2_player_ws_resource_ingest_stream_descriptor).
-define(stream_desc_egest,  rtsv2_player_ws_resource_egest_stream_descriptor).



-record(?state_running,
        { trace_id :: binary_string()
        , public_ip_string :: binary_string()
        , socket_url :: binary_string()
        , stream_desc :: stream_desc_ingest() | stream_desc_egest()
        , start_options :: webrtc_stream_server:start_options()
        , server_id :: term()
        , ice_servers :: list(map())
        , profiles :: list(rtsv2_slot_configuration:slot_profile())
        }).


-record(?state_redirect,
        { alternative_location :: binary_string()
        }).


-record(?state_not_found, {}).


-record(?state_retry, { reason :: stream_initializing }).


-record(?stream_desc_ingest,
        { slot_id :: non_neg_integer()
        , profile_name :: binary_string()
        , stream_role :: binary_string()
        , ingest_key :: term()
        }).
-type stream_desc_ingest() :: #?stream_desc_ingest{}.


-record(?stream_desc_egest,
        { slot_id  :: binary_string()
        , egest_key :: term()
        , start_stream_result :: term()
        , get_slot_configuration :: fun()
        , add_client :: fun()
        }).
-type stream_desc_egest() :: #?stream_desc_egest{}.


-define(MAX_PAYLOAD_SIZE, 16384).
-define(IDLE_TIMEOUT_MS, 60000).


-define(WebSocketStatusCode_GoingAway, 1001).
-define(WebSocketStatusCode_DataInconsistentWithType, 1007).
-define(WebSocketStatusCode_MessageNotImplemented, 4000).
-define(WebSocketStatusCode_MessageBad, 4001).
-define(WebSocketStatusCode_InvalidSDP, 4002).
-define(WebSocketStatusCode_StreamNotFound, 4003).
-define(WebSocketStatusCode_StreamNotReadyRetryLater, 4004).


init(Req,
     #{ mode := egest
      , make_egest_key := MakeEgestKey
      , start_stream := StartStream
      , add_client := AddClient
      , get_slot_configuration := GetSlotConfiguration
      }
    ) ->

  %% Ensure the relay is set-up and get the relay key
  SlotId = binary_to_integer(cowboy_req:binding(slot_id, Req)),

  %% NOTE: StartStream returns an effect, hence the extra invocation
  StartStreamResult =
    (StartStream(SlotId))(),

  io:format(user, "START STREAM RESULT: ~p -> ~p~n~n", [SlotId, StartStreamResult]),

  StreamDesc =
    #?stream_desc_egest{ slot_id =  SlotId
                       , egest_key = MakeEgestKey(SlotId)
                       , start_stream_result = StartStreamResult
                       , get_slot_configuration = GetSlotConfiguration
                       , add_client = AddClient
                       },

  init_prime(Req, StreamDesc);

init(Req,
     #{ mode := ingest
      , make_ingest_key := MakeIngestKey
      }
    ) ->

  SlotId = binary_to_integer(cowboy_req:binding(slot_id, Req)),
  StreamRole = cowboy_req:binding(stream_role, Req),
  ProfileName = cowboy_req:binding(profile_name, Req),

  StreamDesc =
    #?stream_desc_ingest{ slot_id = SlotId
                        , stream_role = StreamRole
                        , profile_name = ProfileName
                        , ingest_key = ((MakeIngestKey(SlotId))(StreamRole))(ProfileName)
                        },

  init_prime(Req, StreamDesc).

init_prime(Req, StreamDesc) ->

  %% Needs to come from config!
  PublicIP = this_server_ip(Req),
  CookieDomainName = PublicIP,
  ServerId = <<"p1n1">>,
  ServerEpoch = <<"1">>,

  TurnIP = PublicIP,
  TurnPort = 4000,
  TurnUsername = <<"username">>,
  TurnPassword = <<"password">>,

  { TraceId, NewReq } = maybe_allocate_trace_id(ServerId, ServerEpoch, CookieDomainName, Req),

  case determine_stream_availability(StreamDesc) of
    available_nowhere ->
      ?LOG_INFO(#{ what => "session.rejected", reason => "stream not found", context => #{}}),

      { cowboy_websocket
      , NewReq
      , #?state_not_found{}
      , #{ max_frame_size => 1024
         , idle_timeout => 500
         }
      };

    initializing_here ->
      ?LOG_INFO(#{ what => "session.rejected", reason => "stream initializing", context => #{}}),

      { cowboy_websocket
      , NewReq
      , #?state_retry{ reason = stream_initializing }
      , #{ max_frame_size => 1024
         , idle_timeout => 500
         }
      };

    { available_elsewhere, [ Alternate | _ ] } ->

      %% TODO: PS: select wss/ws appropriately
      AlternateSocketPath = <<"ws://", Alternate/binary, (cowboy_req:path(Req))/binary>>,

      ?LOG_INFO(#{ what => "session.redirect", reason => "egest available elsewhere", context => #{ target_uri => AlternateSocketPath }}),

      { cowboy_websocket
      , NewReq
      , #?state_redirect{ alternative_location = AlternateSocketPath }
      , #{ max_frame_size => 1024
         , idle_timeout => 1000
         }
      };

    { available_here, Profiles } ->
      PublicIPString = i_convert:convert(PublicIP, binary_string),
      SocketURL = <<"wss://", PublicIPString/binary, (cowboy_req:path(Req))/binary>>,

      { cowboy_websocket
      , NewReq
      , #?state_running{ trace_id = TraceId
                       , public_ip_string = PublicIPString
                       , socket_url = SocketURL
                       , stream_desc = StreamDesc
                       , start_options = construct_start_options(TraceId, PublicIPString, Profiles, StreamDesc)
                       , server_id = construct_server_id(StreamDesc)
                       , ice_servers = stun_turn_config(none, TurnIP, TurnPort, TurnUsername, TurnPassword)
                       , profiles = Profiles
                       }
      , #{ max_frame_size => ?MAX_PAYLOAD_SIZE
         , idle_timeout => ?IDLE_TIMEOUT_MS
         }
      }
  end.


terminate(stop, _PartialReq, #?state_redirect{}) ->
  ok;

terminate(stop, _PartialReq, #?state_not_found{}) ->
  ok;

terminate(stop, _PartialReq, #?state_retry{}) ->
  ok;

terminate(_Reason, _PartialReq, #?state_running{ server_id = ServerId, trace_id = TraceId }) ->
  %% ?LOG_DEBUG(#{ what => "close", reason => Reason }),
  webrtc_stream_server:cast_session(ServerId, TraceId, notify_socket_disconnect),
  ok.

websocket_init(#?state_redirect{ alternative_location = AlternativeSocketPath } = State) ->

  ByeMessage =
    #{ type => bye
     , otherEdges =>
         [ #{ socketURL => AlternativeSocketPath, iceServers => [] }
         ]
     },

  { [ {text, jsx:encode(ByeMessage)}
    , close_frame(?WebSocketStatusCode_GoingAway)
    ]
  , State
  };

websocket_init(#?state_not_found{} = State) ->

  ByeMessage =
    #{ type => bye
     , alternatives => []
     },

  { [ {text, jsx:encode(ByeMessage)}
    , close_frame(?WebSocketStatusCode_StreamNotFound)
    ]
  , State
  };

websocket_init(#?state_retry{ reason = _Reason } = State) ->

  ByeMessage =
    #{ type => bye
     , alternatives => []
     },

  { [ {text, jsx:encode(ByeMessage)}
    , close_frame(?WebSocketStatusCode_StreamNotReadyRetryLater)
    ]
  , State
  };

websocket_init(#?state_running{ trace_id = TraceId
                              , public_ip_string = _PublicIPString
                              , server_id = ServerId
                              , start_options = InitialStartOptions
                              , socket_url = SocketURL
                              , ice_servers = ICEServers
                              , profiles = [ #{ name := ActiveProfileName } | _OtherProfiles ] = Profiles
                              , stream_desc = StreamDesc
                              } = State) ->

  logger:set_process_metadata(#{ correlation_id => TraceId }),

  case StreamDesc of
    #?stream_desc_egest{ slot_id = SlotId
                       , add_client = AddClient } ->
      {right, unit} = (AddClient(self(), SlotId))();
    _ ->
      ok
  end,

  FinalStartOptions =
    InitialStartOptions#{ event_handler =>
                            begin
                              Self = self(),

                              fun(Event) ->
                                  Self ! { session_event, Event }
                              end
                            end
                        },

  NewState = State#?state_running{ start_options = FinalStartOptions },

  webrtc_stream_server:ensure_session(ServerId, TraceId, FinalStartOptions),

  webrtc_stream_server:subscribe_for_msgs(TraceId, #subscription_options{}),

  InitialMessage =
    #{ type => init
     , traceId => TraceId
     , thisEdge =>
         #{ socketURL => SocketURL
          , iceServers => ICEServers
          }
     , activeVariant => ActiveProfileName
     , variants => [ ProfileName || #{ name := ProfileName } <- Profiles ]
     },

  { [ {text, jsx:encode(InitialMessage)} ]
  , NewState
  }.


websocket_handle({ text, JSON }, State) ->
  try
    jsx:decode(JSON, [return_maps])
  of
    #{ <<"type">> := Type } = Message ->

      case Type of
        <<"ping">> ->
          handle_ping(State);

        <<"sdp.offer">> ->
          handle_sdp_offer(Message, State);

        <<"ice.candidate">> ->
          handle_ice_gathering_candidate(Message, State);

        <<"ice.done">> ->
          handle_ice_gathering_done(Message, State);

        _UnknownMessageType ->
          { [ close_frame(?WebSocketStatusCode_MessageNotImplemented) ]
          , State
          }
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

websocket_info({ session_event
               , #{ type := server_ice_candidate
                  , media_line_index := LineIndex
                  , candidate := Candidate
                  }
               },
               State
              ) ->
  { [ json_frame( <<"ice.candidate">>,
                  #{ <<"index">> => LineIndex
                   , <<"candidate">> => Candidate
                   }
                ) ]
  , State
  };

websocket_info(not_implemented, State) ->
  {ok, State}.


%%% ----------------------------------------------------------------------------
%%% Private Functions
%%% ----------------------------------------------------------------------------
determine_stream_availability(#?stream_desc_ingest{ ingest_key = IngestKey, slot_id = SlotId, profile_name = ProfileName }) ->

  BusName = {webrtc_stream_output, IngestKey},

  case pubsub:exists(BusName) of
    true ->

      case pubsub:read_bus_metadata(BusName) of
        { ok, #live_stream_metadata{ program_details = _Frame = #frame{} } } ->

          case rtsv2_slot_media_source_publish_processor:maybe_slot_configuration(SlotId) of
            #{ profiles := Profiles } ->

              case [ Profile || Profile = #{ name := ProfileName } <- Profiles, ProfileName =:= ProfileName ] of
                [ MatchingProfile | _ ] ->
                  { available_here, [ MatchingProfile ] };

                _NoMatchingProfile ->
                  available_nowhere
              end;

            _NoSlotConfigAvailable ->
              initializing_here
          end;

        _NotReceivingMediaYet ->
          initializing_here
      end;

    false ->
      available_nowhere
  end;

determine_stream_availability(#?stream_desc_egest{ start_stream_result = { left, {notFound} } }) ->
  available_nowhere;

determine_stream_availability(#?stream_desc_egest{ start_stream_result = { left, {noResource} } }) ->

  %% TODO: PS: redirect to a different PoP?
  available_nowhere;

determine_stream_availability(#?stream_desc_egest{ start_stream_result = { right, { remote, #{ address := HostName } } } }) ->

  %% TODO: PS: proper determination of port - config?
  Authority = << HostName/binary, ":3000" >>,
  {available_elsewhere, [ Authority ]};

determine_stream_availability(#?stream_desc_egest{ egest_key = EgestKey, get_slot_configuration = GetSlotConfiguration }) ->

  %% TODO: PS: liveness information?
  %% TODO: PS: getting slot config should be part of the stream detection
  %% NOTE: GetSlotConfiguration returns an effect
  case (GetSlotConfiguration(EgestKey))() of
    {just, #{ profiles := Profiles }} ->
      { available_here, Profiles };

    {nothing} ->

      %% TODO: PS: available elsewhere detection
io:format(user, "XXX HERE~n", []),
      available_nowhere
  end.


handle_ping(State) ->
  ?LOG_DEBUG(#{ what => "ping", result => "ok" }),
  { [ json_frame(<<"pong">>) ]
  , State
  }.


handle_sdp_offer(#{ <<"offer">> := SDP }, #?state_running{ server_id = ServerId, trace_id = TraceId, start_options = StartOptions } = State) ->
  case webrtc_stream_server:handle_client_offer(ServerId, TraceId, StartOptions, SDP) of
    {ok, ResponseSDP} ->
      ?LOG_DEBUG(#{ what => "negotiation.offer-received", result => "ok", context => #{ offer => SDP } }),
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


handle_ice_gathering_candidate(#{ <<"candidate">> := Candidate, <<"index">> := MediaLineIndex }, #?state_running{ server_id = ServerId, trace_id = TraceId, start_options = StartOptions } = State) ->
  case webrtc_stream_server:handle_ice_candidate(ServerId, TraceId, StartOptions, MediaLineIndex, Candidate) of
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


maybe_allocate_trace_id(ServerId, ServerEpoch, CookieDomainName, Req) ->
  case cowboy_req:match_cookies([{tid, [], undefined}], Req) of
    #{tid := TraceId} when TraceId =/= undefined ->
      {TraceId, Req};

    _ ->
      TraceId = generate_trace_id(ServerId, ServerEpoch),
      NewReq = cowboy_req:set_resp_cookie(<<"tid">>,
                                          TraceId,
                                          Req,
                                          #{ http_only => true
                                           , path => <<"/">>
                                           , secure => true
                                           , same_site => strict
                                           , domain => CookieDomainName
                                           }
                                         ),
      {TraceId, NewReq}
  end.


generate_trace_id(ThisServerId, ThisServerEpoch) ->
  %% TODO: the locally unique id needs to be non-guessable
  LocallyUniqueId = integer_to_binary(erlang:unique_integer([positive])),
  <<ThisServerId/binary, ".", ThisServerEpoch/binary, ".", LocallyUniqueId/binary>>.


json_frame(Type) ->
  json_frame(Type, undefined).


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


construct_start_options(TraceId, IP, [ SlotProfile ], #?stream_desc_ingest{}) ->

  #{ firstAudioSSRC := AudioSSRC
   , firstVideoSSRC := VideoSSRC
   } = SlotProfile,

  #{ session_id => TraceId
   , local_address => IP
   , handler_module => rtsv2_webrtc_session_handler
   , handler_args => [ TraceId ]

     %% TODO: from config
   , ice_options => #{ resolution_disabled => false
                     , mdns_resolution_disabled => false
                     }

   , rtp_egest_config =>
       { passthrough
       , #{ audio_ssrc => AudioSSRC
          , video_ssrc => VideoSSRC
          }
       }
   };

construct_start_options(TraceId, IP, _SlotProfiles, #?stream_desc_egest{}) ->

  #{ session_id => TraceId
   , local_address => IP
   , handler_module => rtsv2_webrtc_session_handler
   , handler_args => [ TraceId ]

     %% TODO: from config
   , ice_options => #{ resolution_disabled => false
                     , mdns_resolution_disabled => false
                     }

   , rtp_egest_config =>
       { passthrough
       , #{ audio_ssrc => ?EGEST_AUDIO_SSRC
          , video_ssrc => ?EGEST_VIDEO_SSRC
          }
       }
   }.


construct_server_id(#?stream_desc_ingest{ ingest_key = IngestKey }) ->
  IngestKey;

construct_server_id(#?stream_desc_egest{ egest_key = EgestKey }) ->
  EgestKey.
