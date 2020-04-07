-module(rtsv2_player_ws_resource).


-include_lib("kernel/include/inet.hrl").
-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_common/include/id3as_message_bus.hrl").
-include_lib("id3as_rtc/include/webrtc.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include("../rtsv2_types.hrl").
-include("../rtsv2_rtp.hrl").


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


-record(?state_initializing,
        { trace_id :: binary_string()
        , public_ip_string :: binary_string()
        , socket_url :: binary_string()
        , stream_desc :: stream_desc_ingest() | stream_desc_egest()
        , server_id :: term()

        , ice_servers :: list(map())
        }).


-record(?state_running,
        { trace_id :: binary_string()
        , public_ip_string :: binary_string()
        , socket_url :: binary_string()
        , stream_desc :: stream_desc_ingest() | stream_desc_egest()
        , server_id :: term()

        , start_options :: webrtc_stream_server:start_options()
        , profiles :: list(rtsv2_slot_configuration:slot_profile())
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
        , profile_name :: binary_string()
        , slot_role :: binary_string()
        , ingest_key :: term()
        }).
-type stream_desc_ingest() :: #stream_desc_ingest{}.


-record(stream_desc_egest,
        { slot_id  :: slot_id()
        , slot_role :: binary_string()
        , egest_key :: term()
        , start_stream_result :: term()
        , get_slot_configuration :: fun()
        , data_object_send_message :: fun()
        , data_object_update :: fun()
        , add_client :: fun()
        , audio_ssrc :: rtp:ssrc()
        , video_ssrc :: rtp:ssrc()
        }).
-type stream_desc_egest() :: #stream_desc_egest{}.


-define(MAX_PAYLOAD_SIZE, 16384).
-define(IDLE_TIMEOUT_MS, 60000).


-define(WebSocketStatusCode_GoingAway, 1001).
-define(WebSocketStatusCode_DataInconsistentWithType, 1007).
-define(WebSocketStatusCode_MessageNotImplemented, 4000).
-define(WebSocketStatusCode_MessageBad, 4001).
-define(WebSocketStatusCode_InvalidSDP, 4002).
-define(WebSocketStatusCode_StreamNotFound, 4003).
-define(WebSocketStatusCode_StreamNotReadyRetryLater, 4004).


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
      init_prime(Req, StreamDesc)
  end.

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

      %% TODO: PS: select wss/ws appropriately
      AlternateSocketPath = <<"ws://", Alternate/binary, (cowboy_req:path(Req))/binary>>,

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
      SocketURL = <<"wss://", PublicIPString/binary, (cowboy_req:path(Req))/binary>>,

      { cowboy_websocket
      , NewReq
      , #?state_initializing{ trace_id = TraceId
                            , public_ip_string = PublicIPString
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


terminate(_Reason, _PartialReq, #?state_running{ server_id = ServerId, trace_id = TraceId }) ->
  %% ?LOG_DEBUG(#{ what => "close", reason => Reason }),
  webrtc_stream_server:cast_session(ServerId, TraceId, notify_socket_disconnect),
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

websocket_info({egestOnFI, Timestamp, Pts}, State) ->
  { [ json_frame( <<"on-fi">>,
                  #{ <<"timestamp">> => Timestamp
                   , <<"pts">> => Pts }
                ) ]
  , State
  };

websocket_info({egestDataObjectMessage, #{ destination := Destination
                                         , msg := Msg
                                         , sender := Sender }}, State = #?state_running{ trace_id = Id }) ->

  OkToSend = case Destination of
               {broadcast} -> true;
               {private, IdList} -> lists:member(Id, IdList)
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

websocket_info({egestDataObjectUpdateResponse,#{response := Response, %%{ok},
                                                senderRef := SenderRef,
                                                to := To}}, State = #?state_running{ trace_id = Id }) ->
  if
    To == Id ->
      { [ json_frame( <<"dataobject.update-response">>,
                      #{ <<"senderRef">> => SenderRef,
                         <<"response">> => case Response of
                                             {ok} -> <<"ok">>;
                                             {error, {invalidKey, _}} -> <<"invalidKey">>;
                                             {error, {invalidOperation, _}} -> <<"invalidOperation">>;
                                             {error, {compareAndSwapFailed, _}} -> <<"compareAndSwapFailed">>
                                           end
                       }
                    ) ]
        , State
      };
    ?otherwise ->
      {ok, State}
  end;

websocket_info({egestDataObjectBroadcast, #{object := Object}}, State) ->
  { [ json_frame( <<"dataobject.broadcast">>,
                  #{ <<"object">> => dataobject_to_ts(Object)
                   }
                ) ]
  , State
  };

websocket_info(Other, State) ->
  io:format(user, "OTHER: ~p~n", [Other]),
  {ok, State};

websocket_info(not_implemented, State) ->
  {ok, State}.

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
                       , add_client := AddClient
                       }
                     ) ->

  try
    { rtsv2_types:string_to_uuid(cowboy_req:binding(slot_id, Req))
    , cowboy_req:binding(slot_role, Req)
    }
  of
    { SlotId, SlotRole } ->

      { AudioSSRC, VideoSSRC } =
        case SlotRole of
          <<"primary">> ->
            { ?make_audio_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 0)
            , ?make_video_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 0)
            };
          <<"backup">> ->
            { ?make_audio_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 1)
            , ?make_video_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 1)
            }
        end,

      EgestKey = (MakeEgestKey(SlotId))(SlotRole),

      %% NOTE: StartStream returns an effect, hence the extra invocation
      StartStreamResult =
        (StartStream(EgestKey))(),

      io:format(user, "START STREAM RESULT: ~p -> ~p~n~n", [EgestKey, StartStreamResult]),

      StreamDesc =
        #stream_desc_egest{ slot_id =  SlotId
                          , slot_role = SlotRole
                          , egest_key = EgestKey
                          , start_stream_result = StartStreamResult
                          , get_slot_configuration = GetSlotConfiguration
                          , data_object_send_message = DataObjectSendMessage
                          , data_object_update = DataObjectUpdate
                          , add_client = AddClient
                          , audio_ssrc = AudioSSRC
                          , video_ssrc = VideoSSRC
                          },

      StreamDesc
  catch
    error:badarg ->
      undefined
  end;

try_build_stream_desc(Req,
                      #{ mode := ingest
                       , make_ingest_key := MakeIngestKey
                       }
                     ) ->

  try
    { rtsv2_types:string_to_uuid(cowboy_req:binding(slot_id, Req))
    , cowboy_req:binding(slot_role, Req)
    , cowboy_req:binding(profile_name, Req)
    }
  of
    {SlotId, SlotRole, ProfileName} ->
      #stream_desc_ingest{ slot_id = SlotId
                         , slot_role = SlotRole
                         , profile_name = ProfileName
                         , ingest_key = ((MakeIngestKey(SlotId))(SlotRole))(ProfileName)
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

  %% TODO: PS: proper determination of port - config?
  Authority = << HostName/binary, ":3000" >>,
  {available_elsewhere, [ Authority ]};

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

      case [ Profile || Profile = #{ name := ConfigProfileName } <- Profiles, ConfigProfileName =:= IngestProfileName ] of
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


transition_to_running([ #{ name := ActiveProfileName } | _OtherProfiles ] = Profiles,
                      #?state_initializing{ trace_id = TraceId
                                          , public_ip_string = PublicIPString
                                          , socket_url = SocketURL
                                          , stream_desc = StreamDesc
                                          , server_id = ServerId
                                          , ice_servers = ICEServers
                                          }
                     ) ->

  StartOptions = construct_start_options(TraceId, PublicIPString, Profiles, StreamDesc),
  webrtc_stream_server:ensure_session(ServerId, TraceId, StartOptions),
  webrtc_stream_server:subscribe_for_msgs(TraceId, #subscription_options{}),
  ?I_SUBSCRIBE_BUS_MSGS(<<"egest_bus">>),
  case StreamDesc of
    #stream_desc_egest{ egest_key = EgestKey
                      , add_client = AddClient
                      } ->
      {right, unit} = (AddClient(self(), EgestKey))();
    _ ->
      ok
  end,

  NewState =
    #?state_running{ trace_id = TraceId
                   , public_ip_string = PublicIPString
                   , socket_url = SocketURL
                   , stream_desc = StreamDesc
                   , server_id = ServerId
                   , start_options = StartOptions
                   , profiles = Profiles
                   },

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


handle_ping(State) ->
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


handle_set_quality_constraint_configuration(#{ <<"configuration">> := #{ <<"behavior">> := _Behavior, <<"variant">> := Variant } }, #?state_running{ server_id = ServerId, trace_id = TraceId } = State) ->
  %% TODO: PS: logging, actual abr
  rtsv2_webrtc_session_handler:set_active_profile(ServerId, TraceId, Variant),
  { []
  , State
  , hibernate
  }.

handle_data_object_send_message(#{ <<"msg">> := Message
                                 , <<"destination">> := Destination },
                                State = #?state_running { trace_id = Id
                                                        , stream_desc = #stream_desc_egest {
                                                                           egest_key = Key
                                                                          , data_object_send_message = SendMessage
                                                                          } }) ->
  PursMessageDestination = case Destination of
                             #{ <<"tag">> := <<"publisher">> } -> {publisher};
                             #{ <<"tag">> := <<"broadcast">> } -> {broadcast};
                             #{ <<"tag">> := <<"private">>, <<"to">> :=  To } -> {private, To}
                           end,

  PursMessage = #{ sender => Id
                 , destination => PursMessageDestination
                 , msg => Message
                 , ref => make_ref()
                 },

  ((SendMessage(Key))(PursMessage))(),

  {ok, State}.

handle_data_object_update(#{ <<"operation">> := Operation,
                             <<"senderRef">> := SenderRef },
                          State = #?state_running { trace_id = Id
                                                  , stream_desc = #stream_desc_egest {
                                                                     egest_key = EgestKey
                                                                    , data_object_update = DataObjectUpdate
                                                                    } }) ->
  try
    PursOperation = case Operation of
                      #{ <<"tag">> := <<"inc">>
                       , <<"keys">> := Keys
                       , <<"increment">> := Inc
                       , <<"createIfKeyMissing">> := CreateIfKeyMissing } ->

                        {inc, #{ keys => Keys
                               , increment => Inc
                               , createIfKeyMissing => CreateIfKeyMissing
                               }};

                      #{ <<"tag">> := <<"dec">>
                       , <<"keys">> := Keys
                       , <<"decrement">> := Dec
                       , <<"createIfKeyMissing">> := CreateIfKeyMissing } ->

                        {dec, #{ keys => Keys
                               , decrement => Dec
                               , createIfKeyMissing => CreateIfKeyMissing
                               }};

                      #{ <<"tag">> := <<"cas">>
                       , <<"keys">> := Keys
                       , <<"compare">> := Compare
                       , <<"swap">> := Swap
                       , <<"createIfKeyMissing">> := CreateIfKeyMissing } ->

                        {compareAndSwap, #{ keys => Keys
                                          , compare => dataobject_value_to_purs(Compare)
                                          , swap => dataobject_value_to_purs(Swap)
                                          , createIfKeyMissing => CreateIfKeyMissing
                                          }};

                      #{ <<"tag">> := <<"add">>
                       , <<"keys">> := Keys
                       , <<"value">> := Value
                       , <<"failIfKeyPresent">> := FailIfKeyPresent } ->

                        {add, #{ keys => Keys
                               , value => dataobject_value_to_purs(Value)
                               , failIfKeyPresent => FailIfKeyPresent
                               }};

                      #{ <<"tag">> := <<"update">>
                       , <<"keys">> := Keys
                       , <<"value">> := Value
                       , <<"createIfKeyMissing">> := CreateIfKeyMissing } ->

                        {update, #{ keys => Keys
                                  , value => dataobject_value_to_purs(Value)
                                  , createIfKeyMissing => CreateIfKeyMissing
                                  }};

                      #{ <<"tag">> := <<"delete">>
                       , <<"keys">> := Keys
                       , <<"failIfKeyMissing">> := FailIfKeyMissing } ->

                        {delete, #{ keys => Keys
                                  , failIfKeyMissing => FailIfKeyMissing
                                  }};

                      #{ <<"tag">> := <<"list.insert">>
                       , <<"keys">> := Keys
                       , <<"value">> := Value
                       , <<"createIfKeyMissing">> := CreateIfKeyMissing
                       , <<"failIfValuePresent">> := FailIfValuePresent } ->

                        {listInsert, #{ keys => Keys
                                      , value => dataobject_value_to_purs(Value)
                                      , createIfKeyMissing => CreateIfKeyMissing
                                      , failIfValuePresent => FailIfValuePresent
                                      }};

                      #{ <<"tag">> := <<"list.remove">>
                       , <<"keys">> := Keys
                       , <<"value">> := Value
                       , <<"failIfKeyMissing">> := FailIfKeyMissing
                       , <<"failIfValueMissing">> := FailIfValuePresent } ->

                        {listRemove, #{ keys => Keys
                                      , value => dataobject_value_to_purs(Value)
                                      , failIfKeyMissing => FailIfKeyMissing
                                      , failIfValueMissing => FailIfValuePresent
                                      }}
                    end,

    PursMessage = #{ sender => Id
                   , senderRef => SenderRef
                   , operation => PursOperation
                   , ref => make_ref()
                   },

    ((DataObjectUpdate(EgestKey))(PursMessage))(),

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


construct_start_options(TraceId, IP, [ SlotProfile ] = SlotProfiles, #stream_desc_ingest{}) ->

  #{ firstAudioSSRC := AudioSSRC
   , firstVideoSSRC := VideoSSRC
   } = SlotProfile,

  #{ session_id => TraceId
   , local_address => IP
   , handler_module => rtsv2_webrtc_session_handler
   , handler_args => [ TraceId, SlotProfiles, self(), AudioSSRC, VideoSSRC ]

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

construct_start_options(TraceId, IP, SlotProfiles, #stream_desc_egest{ audio_ssrc = AudioSSRC, video_ssrc = VideoSSRC }) ->

  #{ session_id => TraceId
   , local_address => IP
   , handler_module => rtsv2_webrtc_session_handler
   , handler_args => [ TraceId, SlotProfiles, self(), AudioSSRC, VideoSSRC ]

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

get_maybe(Key, Map) ->
  case maps:get(Key, Map, undefined) of
    undefined ->
      {nothing};
    Value ->
      {just, Value}
  end.

dataobject_to_ts(#{map := Map,
                   version := Version}) ->
  #{ <<"version">> => Version
   , <<"map">> => maps:map(fun(_Key, Value) -> dataobject_value_to_ts(Value) end, Map)
   }.

dataobject_value_to_ts({bool, Boolean}) ->
  Boolean;
dataobject_value_to_ts({number, Number}) ->
  Number;
dataobject_value_to_ts({counter, Number}) ->
  Number;
dataobject_value_to_ts({string, String}) ->
  String;
dataobject_value_to_ts({list, List}) ->
  [dataobject_value_to_ts(Item) || Item <- List];
dataobject_value_to_ts({map, Map}) ->
  maps:map(fun(_Key, Value) ->
               dataobject_value_to_ts(Value)
           end,
           Map).

dataobject_value_to_purs(true) ->
  {bool, true};
dataobject_value_to_purs(false) ->
  {bool, false};
dataobject_value_to_purs(Number) when is_number(Number) ->
  {number, Number * 1.0};
dataobject_value_to_purs(String) when is_binary(String) ->
  {string, String};
dataobject_value_to_purs(List) when is_list(List) ->
  {list, [dataobject_value_to_purs(Item) || Item <- List]};
dataobject_value_to_purs(Map) when is_map(Map) ->
  {map, maps:map(fun(Key, Value) when is_binary(Key) ->
                     dataobject_value_to_purs(Value)
                 end,
                 Map)}.
