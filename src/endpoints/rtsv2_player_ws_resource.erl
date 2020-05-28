-module(rtsv2_player_ws_resource).

-include_lib("kernel/include/inet.hrl").
-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_common/include/id3as_message_bus.hrl").
-include_lib("id3as_rtc/include/webrtc.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include("../rtsv2_types.hrl").
-include("../rtsv2_rtp.hrl").
-include("../rtsv2_webrtc.hrl").
-include("../rtsv2_media_gateway_api.hrl").


-export([ notify_profile_switched/2 ]).


-export([ init/2
        , terminate/3
        ]).


-export([ websocket_init/1
        , websocket_handle/2
        , websocket_info/2
        ]).


-define(state_initializing, rtsv2_player_ws_resource_state_initializing).
-define(state_running,      rtsv2_player_ws_resource_state_running).
-define(state_failed,       rtsv2_player_ws_resource_state_failed).

-define(TWO_POWER_16_MINUS_ONE, 65535).
-define(TWO_POWER_48_MINUS_ONE, 281474976710655).

-type trace_id() :: { server_id(), server_scoped_client_id() }.
-type server_id() :: 0..?TWO_POWER_16_MINUS_ONE.
-type server_scoped_client_id() :: 0..?TWO_POWER_48_MINUS_ONE.

-record(?state_initializing,
        { trace_id :: trace_id()
        , webrtc_session_id :: binary_string()
        , public_ip_string :: binary_string()
        , validation :: validation()
        , socket_url :: binary_string()
        , path :: binary_string()
        , stream_desc :: stream_desc_ingest() | stream_desc_egest()
        , server_id :: term()

        , ice_servers :: list(map())
        }).


-record(?state_running,
        { trace_id :: trace_id()
        , webrtc_session_id :: binary_string()
        , public_ip_string :: binary_string()
        , socket_url :: binary_string()
        , validation :: validation()
        , stream_desc :: stream_desc_ingest() | stream_desc_egest()
        , server_id :: term()
        , path :: binary_string()

        , start_options :: webrtc_stream_server:start_options()
        , profiles :: list(rtsv2_slot_configuration:slot_profile())

        , last_stats_event :: undefined | media_gateway_client_statistics_updated_event()
        , last_stats_received_at :: undefined | non_neg_integer()
        }).


-record(?state_failed,
        { detail :: failure_detail()
        }).


-type failure_detail() :: failure_detail_redirect() | failure_detail_not_found() | failure_detail_retry().


-record(failure_detail_redirect,
        { alternative_location :: binary_string()
        }).
-type failure_detail_redirect() :: #failure_detail_redirect{}.


-record(failure_detail_not_found, {}).
-type failure_detail_not_found() :: #failure_detail_not_found{}.


-record(failure_detail_retry, { reason :: stream_initializing }).
-type failure_detail_retry() :: #failure_detail_retry{}.


-record(stream_desc_ingest,
        { slot_id :: slot_id()
        , profile_name :: profile_name()
        , slot_role :: slot_role()
        , ingest_key :: term()
        , use_media_gateway :: boolean()
        , support_port :: non_neg_integer()
        }).
-type stream_desc_ingest() :: #stream_desc_ingest{}.


-record(stream_desc_egest,
        { slot_id  :: slot_id()
        , slot_role :: slot_role()
        , egest_key :: term()
        , start_stream_result :: term()
        , get_slot_configuration :: fun()
        , data_object_send_message :: fun()
        , data_object_update :: fun()
        , stats_update :: fun()
        , add_client :: fun()
        , audio_ssrc :: rtp:ssrc()
        , video_ssrc :: rtp:ssrc()
        , use_media_gateway :: boolean()
        , public_port :: non_neg_integer()
        }).
-type stream_desc_egest() :: #stream_desc_egest{}.

-record(validation,
        { url :: binary_string()
        , initial_cookie :: binary_string()
        , client_ip_string :: binary_string()
        }).
-type validation() :: no_validation | #validation{}.

-define(MAX_PAYLOAD_SIZE, 16384).
-define(IDLE_TIMEOUT_MS, 60000).


-define(WebSocketStatusCode_GoingAway, 1001).
-define(WebSocketStatusCode_DataInconsistentWithType, 1007).
-define(WebSocketStatusCode_MessageNotImplemented, 4000).
-define(WebSocketStatusCode_MessageBad, 4001).
-define(WebSocketStatusCode_InvalidSDP, 4002).
-define(WebSocketStatusCode_StreamNotFound, 4003).
-define(WebSocketStatusCode_StreamNotReadyRetryLater, 4004).
-define(WebSocketStatusCode_AuthenticationFailed, 4005).


notify_profile_switched(ProfileName, WebSocket) ->
  WebSocket ! { profile_switched, ProfileName }.


init(Req, Params) ->
  case try_build_stream_desc(Req, Params) of
    undefined ->
      { cowboy_websocket
      , Req
      , #?state_failed{ detail = #failure_detail_not_found{} }
      , #{ max_frame_size => 1024
        , idle_timeout => 500
        }
      };

    StreamDesc ->
      case init_validation(Req, StreamDesc, 0)  of
        failed -> init_validation_fail(Req);
        Validation -> init_prime(Req, StreamDesc, Validation)
      end
  end.

init_validation(_Req, _StreamDesc, N) when N > 10 ->
  ?SLOG_DEBUG("Egest never received slot configuration"),
  failed;

init_validation(Req, StreamDesc = #stream_desc_egest{ get_slot_configuration = GetSlotConfiguration
                                                    , egest_key = EgestKey
                                                    , start_stream_result = { right, { local, _ } } }, N) ->
  case (GetSlotConfiguration(EgestKey))() of
    {just, #{ subscribeValidation := true } } ->
      ClientIP = client_ip(Req),
      #{ validation_url := ValidationUrl
       , validation_cookie := ValidationCookie } = cowboy_req:match_qs([ {validation_url, [], undefined},
                                                                         {validation_cookie, [], undefined}], Req),
      case perform_validation(ClientIP, ValidationUrl, ValidationCookie) of
        failed -> failed;
        {error, _Error } -> failed;
        { ok, NewCookie } ->
          #validation{ initial_cookie = NewCookie
                     , client_ip_string = i_convert:convert(ClientIP, binary_string)
                     , url = ValidationUrl
                     }
      end;

    {just, #{ subscribeValidation := false } } ->
      no_validation;

    {nothing} ->
      ?SLOG_DEBUG("No slot configuration available"),
      timer:sleep(100),
      init_validation(Req, StreamDesc, N + 1)

  end;

init_validation(_Req, _StreamDesc, _N) ->
  no_validation.

init_validation_fail(Req) ->
  cowboy_req:reply(403, Req),
  {ok, Req, ignored}.

init_prime(Req, StreamDesc, Validation) ->

  %% Needs to come from config!
  PublicIP = this_server_ip(Req),
  CookieDomainName = PublicIP,
  ServerId = 1,

  TurnIP = PublicIP,
  TurnPort = 4000,
  TurnUsername = <<"username">>,
  TurnPassword = <<"password">>,

  Path = cowboy_req:path(Req),

  { TraceId, NewReq } = maybe_allocate_trace_id(ServerId, CookieDomainName, Req),

  case determine_stream_availability(StreamDesc) of
    available_nowhere ->
      ?LOG_INFO(#{ what => "session.rejected", reason => "stream not found", context => #{}}),

      { cowboy_websocket
      , NewReq
      , #?state_failed{ detail = #failure_detail_not_found{} }
      , #{ max_frame_size => 1024
         , idle_timeout => 500
         }
      };

    initializing_here ->
      ?LOG_INFO(#{ what => "session.rejected", reason => "stream initializing", context => #{}}),

      { cowboy_websocket
      , NewReq
      , #?state_failed{ detail = #failure_detail_retry{ reason = stream_initializing} }
      , #{ max_frame_size => 1024
         , idle_timeout => 500
         }
      };

    { available_elsewhere, [ Alternate | _ ] } ->

      Port = case StreamDesc of
               #stream_desc_egest{public_port = Val} -> Val;
               #stream_desc_ingest{support_port = Val} -> Val
             end,

      AlternateSocketPath = endpoint_helpers:make_ws_url(Alternate, Port, Path),

      ?LOG_INFO(#{ what => "session.redirect", reason => "egest available elsewhere", context => #{ target_uri => AlternateSocketPath }}),

      { cowboy_websocket
      , NewReq
      , #?state_failed{ detail = #failure_detail_redirect{ alternative_location = AlternateSocketPath} }
      , #{ max_frame_size => 1024
         , idle_timeout => 1000
         }
      };

    available_here ->
      PublicIPString = i_convert:convert(PublicIP, binary_string),
      Port = case StreamDesc of
               #stream_desc_egest{public_port = Val} -> Val;
               #stream_desc_ingest{support_port = Val} -> Val
             end,
      SocketURL = endpoint_helpers:make_ws_url(PublicIPString, Port, Path),

      { cowboy_websocket
      , NewReq
      , #?state_initializing{ trace_id = TraceId
                            , webrtc_session_id = format_trace_id(TraceId)
                            , public_ip_string = PublicIPString
                            , path = Path
                            , validation = Validation
                            , socket_url = SocketURL
                            , stream_desc = StreamDesc
                            , server_id = construct_server_id(StreamDesc)
                            , ice_servers = stun_turn_config(none, TurnIP, TurnPort, TurnUsername, TurnPassword)
                            }
      , #{ max_frame_size => ?MAX_PAYLOAD_SIZE
         , idle_timeout => ?IDLE_TIMEOUT_MS
         }
      }
  end.


terminate(Reason, _PartialReq, #?state_running{ server_id = ServerId, webrtc_session_id = SessionId }) ->
  ?SLOG_DEBUG("Terminating", #{ what => "close", reason => Reason }),
  webrtc_stream_server:cast_session(ServerId, SessionId, notify_socket_disconnect),
  ok;

terminate(stop, _PartialReq, #?state_failed{}) ->
  ok;

terminate(Reason, _PartialReq, State) when is_tuple(State) ->

  %% TODO: PS: logging fails in terminate for some reason...
  io:format(user,
            "~p~n",
            [ #{ what => "close"
              , reason => Reason
              , context => #{ state_type => element(1, State) }
              }
            ]),
  ok;

terminate(Reason, _PartialReq, _State) ->

  %% TODO: PS: logging fails in terminate for some reason...
  io:format(user,
            "~p~n",
            [ #{ what => "close"
              , reason => Reason
              , context => #{ state_type => init }
              }
            ]),
  ok.

websocket_init(#?state_failed{ detail = #failure_detail_redirect{ alternative_location = AlternativeSocketPath } } = State) ->

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

websocket_init(#?state_failed{ detail = #failure_detail_not_found{} } = State) ->

  ByeMessage =
    #{ type => bye
     , alternatives => []
     },

  { [ {text, jsx:encode(ByeMessage)}
    , close_frame(?WebSocketStatusCode_StreamNotFound)
    ]
  , State
  };

websocket_init(#?state_failed{ detail = #failure_detail_retry{ reason = _Reason } } = State) ->

  ByeMessage =
    #{ type => bye
     , alternatives => []
     },

  { [ {text, jsx:encode(ByeMessage)}
    , close_frame(?WebSocketStatusCode_StreamNotReadyRetryLater)
    ]
  , State
  };

websocket_init(#?state_initializing{ trace_id = TraceId } = State) ->
  logger:set_process_metadata(#{ correlation_id => TraceId }),
  try_initialize(State).


websocket_info(try_initialize, State) ->
  try_initialize(State);

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

websocket_info({profile_switched, ProfileName}, State) ->
  { [ json_frame( <<"quality-change">>,
                  #{ <<"activeVariant">> => ProfileName }
                ) ]
  , State
  };

websocket_info({egestOnFI, Payload, Pts}, State) ->
  { [ json_frame( <<"on-fi">>,
                  #{ <<"payload">> => Payload
                   , <<"pts">> => Pts }
                ) ]
  , State
  };

websocket_info({egestDrain, Phase, NumPhases, Alternates}, State = #?state_running{ trace_id = {_ServerId, ClientId}
                                                                                  , stream_desc = #stream_desc_egest{public_port = Port}
                                                                                  , path = Path})
  when ClientId rem NumPhases == Phase ->

  OtherEdges = [begin
                  AlternateSocketPath = endpoint_helpers:make_ws_url(Alternate, Port, Path),
                  #{ socketURL => AlternateSocketPath, iceServers => [] }
                end || Alternate <- Alternates],

  ByeMessage =
    #{ type => bye
     , otherEdges => OtherEdges
     },

  { [ {text, jsx:encode(ByeMessage)}
    , close_frame(?WebSocketStatusCode_GoingAway)
    ]
  , State
  };

websocket_info({egestDrain, _Phase, _NumPhases, _Alternates}, State) ->
  {ok, State};

websocket_info({egestDataObjectMessage, #{ destination := Destination
                                         , msg := Msg
                                         , sender := Sender }}, State = #?state_running{ trace_id = TraceId }) ->

  TraceIdString = format_trace_id(TraceId),

  OkToSend = case Destination of
               {broadcast} -> true;
               {private, IdList} -> lists:member(TraceIdString, IdList)
             end,

  if
    OkToSend ->
      {[ json_frame( <<"dataobject.message">>,
                     #{ <<"sender">> => Sender
                      , <<"msg" >> => Msg
                      }
                   ) ]
      , State};
    ?otherwise ->
      {ok, State}
  end;

websocket_info({egestDataObjectUpdateResponse,#{ response := Response
                                               , senderRef := SenderRef
                                               , to := To}}, State = #?state_running{ trace_id = TraceId }) ->

  TraceIdString = format_trace_id(TraceId),

  if
    To == TraceIdString ->
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

websocket_info({egestDataObjectBroadcast, Object}, State) ->
  { [ json_frame( <<"dataobject.broadcast">>,
                  #{ <<"object">> => endpoint_helpers:dataobject_to_ts(Object)
                   }
                ) ]
  , State
  };

websocket_info({egestCurrentActiveProfiles, ActiveProfiles}, State) ->
  { [ json_frame( <<"active-profiles">>,
                  #{ <<"activeProfiles">> => ActiveProfiles
                   }
                ) ]
  , State
  };

websocket_info(#media_gateway_event{ details = Event }, #?state_running{ profiles = Profiles
                                                                       , webrtc_session_id = SessionId
                                                                       , stream_desc = #stream_desc_egest{ egest_key = Key
                                                                                                         , stats_update = StatsUpdate }} = State) ->
  case Event of
    #media_gateway_client_add_failed_event{ reason = Reason } ->
      ?WARNING("The client could not be added to the media gateway for reason ~p. Shutting down.", [Reason]),

      %% TODO: redirects etc
      { [ close_frame(?WebSocketStatusCode_GoingAway) ]
      , State
      };

    #media_gateway_client_synchronization_established_event{ rtp_timestamp = RTPTimestamp } ->
      { [ json_frame( <<"time-zero">>,
                      #{ <<"rtpTimestamp">> => RTPTimestamp
                       }
                    )
        ]
      , State
      };

    #media_gateway_client_subscription_switched_event{ audio_ssrc = AudioSSRC, video_ssrc = VideoSSRC } ->

      MatchingProfile = lists:search(fun(#{ firstAudioSSRC := AudioSSRCMatch, firstVideoSSRC := VideoSSRCMatch}) ->
                                         AudioSSRC =:= AudioSSRCMatch andalso
                                           VideoSSRC =:= VideoSSRCMatch
                                     end,
                                     Profiles
                                    ),

      case MatchingProfile of
        false ->
          ?ERROR("Received notice of switch to profile with audio SSRC ~p and video SSRC ~p, but no matching profile was found.", [AudioSSRC, VideoSSRC]),
          {ok, State};

        {value, #{ profileName := ProfileName }} ->

          { [ json_frame( <<"quality-change">>,
                          #{ <<"activeVariant">> => ProfileName }
                        ) ]
          , State
          }
      end;

    #media_gateway_client_statistics_updated_event{ audio_packets_sent = AudioPacketsSent
                                                  , audio_octets_sent = AudioOctetsSent
                                                  , video_packets_sent = VideoPacketsSent
                                                  , video_octets_sent = VideoOctetsSent
                                                  } = Stats ->

      (StatsUpdate(Key, #{ sessionId => SessionId
                         , audioPacketsSent => AudioPacketsSent
                         , audioOctetsSent => AudioOctetsSent
                         , videoPacketsSent => VideoPacketsSent
                         , videoOctetsSent => VideoOctetsSent}))(),
      { ok
      , State#?state_running{ last_stats_event = Stats
                            , last_stats_received_at = ?vm_now_ms
                            }
      };

    _Other ->
      ?WARNING("Unhandled media gateway event: ~p", [Event]),
      {ok, State}
  end;

websocket_info(not_implemented, State) ->
  {ok, State}.

websocket_handle(ping, State) ->
  {ok, State};

websocket_handle({ text, JSON }, State) ->
  try
    jsx:decode(JSON, [return_maps])
  of
    #{ <<"type">> := Type } = Message ->

      case Type of
        <<"ping">> ->
          handle_ping(Message, State);

        <<"sdp.offer">> ->
          handle_sdp_offer(Message, State);

        <<"ice.candidate">> ->
          handle_ice_gathering_candidate(Message, State);

        <<"ice.done">> ->
          handle_ice_gathering_done(Message, State);

        <<"set-quality-constraint-configuration">> ->
          handle_set_quality_constraint_configuration(Message, State);

        <<"dataobject.send-message">> ->
          handle_data_object_send_message(Message, State);

        <<"dataobject.update">> ->
          handle_data_object_update(Message, State);

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


%%% ----------------------------------------------------------------------------
%%% Private Functions
%%% ----------------------------------------------------------------------------
try_build_stream_desc(Req,
                      #{ mode := egest
                       , make_egest_key := MakeEgestKey
                       , start_stream := StartStream
                       , get_slot_configuration := GetSlotConfiguration
                       , data_object_send_message := DataObjectSendMessage
                       , data_object_update := DataObjectUpdate
                       , stats_update := StatsUpdate
                       , add_client := AddClient
                       , use_media_gateway := UseMediaGateway
                       , public_port := PublicPort
                       }
                     ) ->

  try
    { rtsv2_types:string_to_uuid(cowboy_req:binding(slot_id, Req))
    , rtsv2_types:string_to_slot_role(cowboy_req:binding(slot_role, Req))
    }
  of
    { SlotId, SlotRole } ->

      { AudioSSRC, VideoSSRC } =
        case SlotRole of
          {primary} ->
            { ?make_audio_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 0)
            , ?make_video_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 0)
            };
          {backup} ->
            { ?make_audio_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 1)
            , ?make_video_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 1)
            }
        end,

      EgestKey = (MakeEgestKey(SlotId))(SlotRole),

      %% NOTE: StartStream returns an effect, hence the extra invocation
      StartStreamResult =
        (StartStream(EgestKey))(),

      StreamDesc =
        #stream_desc_egest{ slot_id =  SlotId
                          , slot_role = SlotRole
                          , egest_key = EgestKey
                          , start_stream_result = StartStreamResult
                          , get_slot_configuration = GetSlotConfiguration
                          , data_object_send_message = DataObjectSendMessage
                          , data_object_update = DataObjectUpdate
                          , stats_update = StatsUpdate
                          , add_client = AddClient
                          , audio_ssrc = AudioSSRC
                          , video_ssrc = VideoSSRC
                          , use_media_gateway = UseMediaGateway
                          , public_port = PublicPort
                          },

      StreamDesc
  catch
    error:badarg ->
      undefined
  end;

try_build_stream_desc(Req,
                      #{ mode := ingest
                       , make_ingest_key := MakeIngestKey
                       , use_media_gateway := UseMediaGateway
                       }
                     ) ->

  try
    { rtsv2_types:string_to_uuid(cowboy_req:binding(slot_id, Req))
    , rtsv2_type:string_to_slot_role(cowboy_req:binding(slot_role, Req))
    , cowboy_req:binding(profile_name, Req)
    }
  of
    {SlotId, SlotRole, ProfileName} ->
      #stream_desc_ingest{ slot_id = SlotId
                         , slot_role = SlotRole
                         , profile_name = ProfileName
                         , ingest_key = ((MakeIngestKey(SlotId))(SlotRole))(ProfileName)
                         , use_media_gateway = UseMediaGateway
                         }
  catch
    error:badarg ->
      undefined
  end.


determine_stream_availability(#stream_desc_ingest{ ingest_key = IngestKey }) ->

  BusName = {webrtc_stream_output, IngestKey},

  case pubsub:exists(BusName) of
    true ->

      case pubsub:read_bus_metadata(BusName) of
        { ok, #live_stream_metadata{ program_details = _Frame = #frame{} } } ->
          available_here;

        _NotReceivingMediaYet ->
          available_nowhere
      end;

    false ->
      available_nowhere
  end;

determine_stream_availability(#stream_desc_egest{ start_stream_result = { left, {notFound} } }) ->
  available_nowhere;

determine_stream_availability(#stream_desc_egest{ start_stream_result = { left, {noResource} } }) ->

  %% TODO: PS: redirect to a different PoP?
  available_nowhere;

determine_stream_availability(#stream_desc_egest{ start_stream_result = { right, { remote, #{ address := HostName } } } }) ->

  {available_elsewhere, [ HostName ]};

determine_stream_availability(#stream_desc_egest{}) ->
  available_here.

try_initialize(#?state_initializing{ stream_desc = StreamDesc } = State) ->
  case get_slot_profiles(StreamDesc) of
    undefined ->
      {ok, _TimerRef} = timer:send_after(30, try_initialize),
      {ok, State};

    SlotProfiles ->
      transition_to_running(SlotProfiles, State)
  end.


get_slot_profiles(#stream_desc_ingest{ slot_id = SlotId, profile_name = IngestProfileName }) ->
  case rtsv2_slot_media_source_publish_processor:maybe_slot_configuration(SlotId) of
    #{ profiles := Profiles } ->

      case [ Profile || Profile = #{ profileName := ConfigProfileName } <- Profiles, ConfigProfileName =:= IngestProfileName ] of
        [ MatchingProfile | _ ] ->
          [ MatchingProfile ];

        _NoMatchingProfile ->
          undefined
      end;

    _ ->
      undefined
  end;

get_slot_profiles(#stream_desc_egest{ egest_key = EgestKey, get_slot_configuration = GetSlotConfiguration }) ->
  case (GetSlotConfiguration(EgestKey))() of
    {just, #{ profiles := Profiles }} ->
      Profiles;

    {nothing} ->
      undefined
  end.


transition_to_running([ #{ profileName := ActiveProfileName } | _OtherProfiles ] = Profiles,
                      #?state_initializing{ trace_id = TraceId
                                          , path = Path
                                          , webrtc_session_id = WebRTCSessionId
                                          , public_ip_string = PublicIPString
                                          , validation = Validation
                                          , socket_url = SocketURL
                                          , stream_desc = StreamDesc
                                          , server_id = ServerId
                                          , ice_servers = ICEServers
                                          }
                     ) ->

  StartOptions = construct_start_options(TraceId, WebRTCSessionId, PublicIPString, Profiles, StreamDesc),
  webrtc_stream_server:ensure_session(ServerId, WebRTCSessionId, StartOptions),
  webrtc_stream_server:subscribe_for_msgs(WebRTCSessionId, #subscription_options{}),
  case StreamDesc of
    #stream_desc_egest{ egest_key = EgestKey
                      , add_client = AddClient
                      , slot_id = SlotId
                      , slot_role = SlotRole
                      } ->

      ?I_SUBSCRIBE_BUS_MSGS({egestBus, {egestKey, SlotId, SlotRole}}),
      ?I_SUBSCRIBE_BUS_MSGS({media_gateway_event, trace_id_to_media_gateway_id(TraceId)}),
      {right, unit} = (AddClient(self(), EgestKey, WebRTCSessionId))();
    _ ->
      ok
  end,

  NewState =
    #?state_running{ trace_id = TraceId
                   , webrtc_session_id = WebRTCSessionId
                   , public_ip_string = PublicIPString
                   , path = Path
                   , validation = Validation
                   , socket_url = SocketURL
                   , stream_desc = StreamDesc
                   , server_id = ServerId
                   , start_options = StartOptions
                   , profiles = Profiles
                   },

  InitialMessage =
    #{ type => init
     , traceId => format_trace_id(TraceId)
     , thisEdge =>
         #{ socketURL => SocketURL
          , iceServers => ICEServers
          }
     , activeVariant => ActiveProfileName
     , variants => [ ProfileName || #{ name := ProfileName } <- Profiles ]
     , validationCookie => case Validation of
                            #validation{ initial_cookie = Cookie } -> ?null_coalesce(Cookie, null);
                            _ -> null
                           end
     },

  { [ {text, jsx:encode(InitialMessage)} ]
  , NewState
  }.


handle_ping(#{ <<"validationCookie">> := ClientCookie }, State = #?state_running { validation = #validation{ client_ip_string = ClientIP, url = URL } }) when ClientCookie /= null ->
  case perform_validation(ClientIP, URL, ClientCookie) of
    { ok, NewCookie } ->
      { [ json_frame(<<"pong">>, #{ validationCookie => ?null_coalesce(NewCookie, null) }) ]
      , State
      };
    _ ->
        ByeMessage =
          #{ type => bye
          , alternatives => []
          },

        { [ {text, jsx:encode(ByeMessage)}
          , close_frame(?WebSocketStatusCode_AuthenticationFailed)
          ]
        , State
        }
  end;

handle_ping(_, State) ->
  { [ json_frame(<<"pong">>) ]
  , State
  }.

perform_validation(_IP, undefined, _MaybeCookie) ->
  failed;
perform_validation(IP, URL, MaybeCookie) when is_tuple(IP) ->
  perform_validation(i_convert:convert(IP, binary_string), URL, MaybeCookie);
perform_validation(IP, URL, MaybeCookie) ->
  ValidHost = case http_uri:parse(URL) of
                {ok, {_Scheme, _UserInfo, Host, _Port, _Path, _Query}} ->
                  %% TODO take from config
                  case Host of
                    <<"subscribe-validator.rts.llnwi.net">> -> true;
                    <<"172.16.171.1">> -> true;
                    _ -> false
                  end;
                Error ->
                  ?SLOG_DEBUG("invalid url", #{url => URL,
                                               error => Error}),
                  invalid_url
              end,

  case ValidHost of
    false -> { error, bad_host };
    invalid_url -> { error, invalid_url };
    true ->

      Res = spud_gun:get(URL, [{<<"X-LLNW-Auth-IP">>, IP}]
                                ++ case MaybeCookie of
                                    undefined -> [];
                                    ExistingCookie -> [{"cookie", ExistingCookie}]
                                  end),

      case Res of
        {ok, 200, RespHeaders, _Body} ->
          case lists:keyfind(<<"set-cookie">>, 1, RespHeaders) of
            false ->
              { ok, undefined };
            {_, CookieValue} ->
              case string:split(CookieValue, ";", all) of
                [ Cookie | _ ] ->
                  { ok, Cookie };
                _ ->
                  ?SLOG_DEBUG("bad cookie~n", #{}),
                  { error, bad_cookie }
              end
          end;
        {ok, 400, _RespHeaders, _Body} ->
          ?SLOG_DEBUG("cookie rejected - 400 response", #{url => URL}),
          failed;
        Response ->
          ?SLOG_DEBUG("non 200 response", #{response => Response}),
          {error, {response, Response}}
      end
  end.


handle_sdp_offer(#{ <<"offer">> := SDP }, #?state_running{ server_id = ServerId, webrtc_session_id = WebRTCSessionId, start_options = StartOptions } = State) ->
  case webrtc_stream_server:handle_client_offer(ServerId, WebRTCSessionId, StartOptions, SDP) of
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


handle_ice_gathering_candidate(#{ <<"candidate">> := Candidate, <<"index">> := MediaLineIndex }, #?state_running{ server_id = ServerId, webrtc_session_id = WebRTCSessionId, start_options = StartOptions } = State) ->
  case webrtc_stream_server:handle_ice_candidate(ServerId, WebRTCSessionId, StartOptions, MediaLineIndex, Candidate) of
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


handle_set_quality_constraint_configuration(#{ <<"configuration">> := #{ <<"behavior">> := _Behavior, <<"variant">> := Variant } }, #?state_running{ server_id = ServerId, webrtc_session_id = WebRTCSessionId } = State) ->
  %% TODO: PS: logging, actual abr
  rtsv2_webrtc_session_handler:set_active_profile(ServerId, WebRTCSessionId, Variant),
  { []
  , State
  , hibernate
  }.

handle_data_object_send_message(#{ <<"msg">> := Message
                                 , <<"destination">> := Destination },
                                State = #?state_running { trace_id = TraceId
                                                        , stream_desc = #stream_desc_egest {
                                                                           egest_key = Key
                                                                          , data_object_send_message = SendMessage
                                                                          } }) ->

  try
    PursMessage = endpoint_helpers:dataobject_message_to_purs(format_trace_id(TraceId), Message, Destination),

    ((SendMessage(Key))(PursMessage))(),

    {ok, State}
  catch
    _:_ ->
      { [ json_frame( <<"dataobject.message-failure">>,
                      #{ } ) ]
      , State
      }
  end.

handle_data_object_update(#{ <<"operation">> := Operation,
                             <<"senderRef">> := SenderRef },
                          State = #?state_running { trace_id = TraceId
                                                  , stream_desc = #stream_desc_egest {
                                                                     egest_key = EgestKey
                                                                    , data_object_update = DataObjectUpdate
                                                                    } }) ->
  try
    PursOperation = endpoint_helpers:dataobject_operation_to_purs(Operation),
    PursMessage = #{ sender => format_trace_id(TraceId)
                   , senderRef => SenderRef
                   , operation => PursOperation
                   , ref => make_ref()
                   },

    ((DataObjectUpdate(EgestKey))(PursMessage))(),

    {ok, State}
  catch
    _Class:_Reason ->
      { [ json_frame( <<"dataobject.update-response">>,
                      #{ <<"senderRef">> => SenderRef,
                         <<"response">> => <<"invalidRequest">>} ) ]
      , State
      }
  end;

handle_data_object_update(_Request = #{ <<"senderRef">> := SenderRef }, State) ->
  { [ json_frame( <<"dataobject.update-response">>,
                  #{ <<"senderRef">> => SenderRef,
                     <<"response">> => <<"invalidRequest">>} ) ]
  , State
  };

handle_data_object_update(_Request, State) ->
  { [ json_frame( <<"dataobject.update-response">>,
                  #{ <<"senderRef">> => <<"unknown">>,
                     <<"response">> => <<"invalidRequest">>} ) ]
  , State
  }.


maybe_allocate_trace_id(ServerId, CookieDomainName, Req) ->
  case cowboy_req:match_cookies([{tid, [], undefined}], Req) of
    #{tid := TraceIdString} when TraceIdString =/= undefined ->

      try
        parse_trace_id(TraceIdString)
      of
        TraceId ->
          {TraceId, Req}
      catch
        _C:_E ->
          allocate_trace_id(ServerId, CookieDomainName, Req)
      end;

    _ ->
      allocate_trace_id(ServerId, CookieDomainName, Req)
  end.


allocate_trace_id(ServerId, CookieDomainName, Req) ->
  TraceId = generate_trace_id(ServerId),
  TraceIdString = format_trace_id(TraceId),
  NewReq = cowboy_req:set_resp_cookie(<<"tid">>,
                                      TraceIdString,
                                      Req,
                                      #{ http_only => true
                                       , path => <<"/">>
                                       , secure => true
                                       , same_site => strict
                                       , domain => CookieDomainName
                                       }
                                     ),
  {TraceId, NewReq}.


generate_trace_id(ThisServerId) ->
  << LocallyUniqueId:48/big-integer >> = crypto:strong_rand_bytes(6),
  { ThisServerId, LocallyUniqueId }.


format_trace_id({ServerId, ServerScopedClientId}) ->
  <<(integer_to_binary(ServerId))/binary, ".",
    (integer_to_binary(ServerScopedClientId))/binary
  >>.


parse_trace_id(TraceIdString) ->
  [ServerIdString, ServerScopedClientIdString] = binary:split(TraceIdString, <<".">>, [global]),

  TraceId =
    { binary_to_integer(ServerIdString)
    , binary_to_integer(ServerScopedClientIdString)
    },

  case TraceId of
    { ServerId, ServerScopedClientId } when
        (ServerId >= 0 andalso ServerId =< ?TWO_POWER_16_MINUS_ONE) andalso
        (ServerScopedClientId >= 0 andalso ServerScopedClientId =< ?TWO_POWER_48_MINUS_ONE) ->
      TraceId
  end.


trace_id_to_media_gateway_id({ServerId, ServerScopedClientId}) ->
  (ServerId bsl 48) bor ServerScopedClientId.


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

client_ip(Req) ->
  {ClientIp, _} = endpoint_helpers:client_address(Req),
  ClientIp.

construct_start_options(TraceId,
                        WebRTCSessionId,
                        IP,
                        [ SlotProfile ] = SlotProfiles,
                        #stream_desc_ingest{ slot_id = SlotId, slot_role = SlotRole, use_media_gateway = UseMediaGateway }
                       ) ->

  #{ firstAudioSSRC := AudioSSRC
   , firstVideoSSRC := VideoSSRC
   } = SlotProfile,

  #{ session_id => WebRTCSessionId
   , local_address => IP
   , handler_module => rtsv2_webrtc_session_handler
   , handler_args =>
       #rtsv2_webrtc_session_handler_config{ session_id = WebRTCSessionId
                                           , cname = WebRTCSessionId
                                           , media_gateway_client_id = trace_id_to_media_gateway_id(TraceId)
                                           , slot_id = SlotId
                                           , slot_role = SlotRole
                                           , profiles = SlotProfiles
                                           , web_socket = self()
                                           , audio_ssrc = AudioSSRC
                                           , video_ssrc = VideoSSRC
                                           , use_media_gateway = UseMediaGateway
                                           }

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

   , event_handler =>
       begin
         Self = self(),

         fun(Event) ->
             Self ! { session_event, Event }
         end
       end
   };

construct_start_options(TraceId,
                        WebRTCSessionId,
                        IP,
                        SlotProfiles,
                        #stream_desc_egest{ slot_id = SlotId
                                          , slot_role = SlotRole
                                          , audio_ssrc = AudioSSRC
                                          , video_ssrc = VideoSSRC
                                          , use_media_gateway = UseMediaGateway
                                          }
                       ) ->

  #{ session_id => WebRTCSessionId
   , local_address => IP
   , handler_module => rtsv2_webrtc_session_handler
   , handler_args =>
       #rtsv2_webrtc_session_handler_config{ session_id = WebRTCSessionId
                                           , cname = WebRTCSessionId
                                           , media_gateway_client_id = trace_id_to_media_gateway_id(TraceId)
                                           , slot_id = SlotId
                                           , slot_role = SlotRole
                                           , profiles = SlotProfiles
                                           , web_socket = self()
                                           , audio_ssrc = AudioSSRC
                                           , video_ssrc = VideoSSRC
                                           , use_media_gateway = UseMediaGateway
                                           }

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

   , event_handler =>
       begin
         Self = self(),

         fun(Event) ->
             Self ! { session_event, Event }
         end
       end
   }.


construct_server_id(#stream_desc_ingest{ ingest_key = IngestKey }) ->
  IngestKey;

construct_server_id(#stream_desc_egest{ egest_key = EgestKey }) ->
  EgestKey.
