-module(rtsv2_webrtc_session_handler).

-behaviour(webrtc_session_handler).

-export([ set_active_profile/3
        , set_active_profiles/3
        , set_quality_constraint/4
        ]).

-export([ init/1
        , handle_media_frame/2
        , handle_info/2
        , handle_cast/2
        ]).

-include_lib("id3as_rtc/include/rtp.hrl").
-include_lib("id3as_rtc/include/rtp_engine.hrl").
-include_lib("id3as_rtc/include/webrtc.hrl").
-include_lib("id3as_rtc/include/rtcp.hrl").
-include("./rtsv2_rtp.hrl").
-include("./rtsv2_types.hrl").
-include("./rtsv2_webrtc.hrl").
-include("./rtsv2_media_gateway_api.hrl").

-define(state, ?MODULE).

-record(egest_stream_state,
        { active_ssrc
        , last_sequence = 16
        , last_timestamp
        , timestamp_delta = 0
        , egest_ssrc
        }).

-record(desired_state,
        { profile_name
        , desired_video_ssrc
        , desired_audio_ssrc
        }).

-record(?state,
        { session_id :: binary_string()
        , cname :: binary_string()
        , media_gateway_client_id :: non_neg_integer()
        , audio_only :: boolean()

        , profiles :: list(slot_profile())
        , valid_profiles :: list(slot_profile())
        , quality_constraint_behaviour :: max_quality | force_quality
        , quality_constraint_profile :: binary_string()
        , active_profile_name :: binary_string()

        , web_socket :: pid()
        , slot_id :: slot_id()
        , slot_role :: slot_role()

        , desired_state = undefined

        , video_state :: #egest_stream_state{}
        , audio_state :: #egest_stream_state{}

        , video_stream_element_config :: undefined | media_gateway_stream_element_config()
        , audio_stream_element_config :: undefined | media_gateway_stream_element_config()

        , use_media_gateway :: boolean()
        }).

set_active_profile(ServerId, TraceId, ProfileName) ->
  webrtc_stream_server:cast_session(ServerId, TraceId, {set_active_profile, ProfileName}).

set_active_profiles(ServerId, TraceId, ProfileNames) ->
  webrtc_stream_server:cast_session(ServerId, TraceId, {set_active_profiles, ProfileNames}).

set_quality_constraint(ServerId, TraceId, Behaviour, ProfileName) ->
  webrtc_stream_server:cast_session(ServerId, TraceId, {set_quality_constraint, Behaviour, ProfileName}).

init(#rtsv2_webrtc_session_handler_config{ session_id = SessionId
                                         , cname = CName
                                         , media_gateway_client_id = MediaGatewayClientId
                                         , slot_id = SlotId
                                         , slot_role = SlotRole
                                         , profiles = [ #{ profileName := ActiveProfileName } | _ ] = Profiles
                                         , active_profile_names = ActiveProfileNames
                                         , audio_only = AudioOnly
                                         , web_socket = WebSocket
                                         , audio_ssrc = AudioSSRC
                                         , video_ssrc = VideoSSRC
                                         , use_media_gateway = UseMediaGateway
                                         }
     ) ->

  ?DEBUG("Session handler started for session ~p in profile ~p and mode ~p",
         [ SessionId
         , ActiveProfileName
         , case AudioOnly of
             true ->
               <<"Audio Only">>;
             false ->
               <<"Audio/Video">>
           end
         ]),

  ProfilesWithActiveFlag = [maps:put(isActive, lists:member(Name, ActiveProfileNames), P) || P = #{profileName := Name} <- Profiles],

  State1 = #?state{ session_id = SessionId
                  , cname = CName
                  , media_gateway_client_id = MediaGatewayClientId
                  , slot_id = SlotId
                  , slot_role = SlotRole

                  , profiles = ProfilesWithActiveFlag
                  , valid_profiles = ProfilesWithActiveFlag
                  , active_profile_name = ActiveProfileName
                  , quality_constraint_behaviour = max_quality
                  , quality_constraint_profile = ActiveProfileName

                  , audio_only = AudioOnly
                  , web_socket = WebSocket
                  , audio_state = #egest_stream_state{ egest_ssrc = AudioSSRC }
                  , video_state = #egest_stream_state{ egest_ssrc = VideoSSRC }
                  , use_media_gateway = UseMediaGateway
                  },
  State2 = set_active_profile_impl(State1),
  State2.

handle_media_frame(_, State = #?state{use_media_gateway = false}) ->
  {ok, State};

handle_media_frame(#rtp_engine_msg{message = #rtp_engine_remb{}}, #?state{ quality_constraint_behaviour = force_quality } = State) ->
  %% ?DEBUG("Received bandwidth estimate, but we're in force-quality mode."),
  {ok, State};

handle_media_frame(#rtp_engine_msg{message = #rtp_engine_remb{ feedback = #rtcp_remb_payload_feedback_message{ estimated_bandwidth_bps = Bandwidth }}},
                   #?state{ quality_constraint_behaviour = max_quality
                          , valid_profiles = ValidProfiles
                          , active_profile_name = ActiveProfileName
                          } = State
                  ) ->

  case choose_profile_for_bandwidth(Bandwidth, ValidProfiles) of
    ChosenProfileName when ChosenProfileName =:= ActiveProfileName ->
      %% ?DEBUG("Received bandwidth estimate (~pbps), but we decided to stay as we are with profile ~p.", [Bandwidth, ChosenProfileName]),
      {ok, State};

    ChosenProfileName ->
      %% ?DEBUG("Received bandwidth estimate (~pbps), and we decided to change to profile ~p.", [Bandwidth, ChosenProfileName]),
      {ok, set_active_profile_impl(State#?state{active_profile_name = ChosenProfileName})}
  end;

handle_media_frame(#rtp_engine_msg{message = _RTPEngineState}, State) ->
  {ok, State};

handle_media_frame(#webrtc_media_channel_message{ message = #webrtc_media_channel_ready{ channel_type = audio
                                                                                       , media_socket = Socket
                                                                                       , egest_crypto = EgestCrypto
                                                                                       , rtp_engine_snapshot = RTPEngineSnapshot
                                                                                       }
                                                },
                   #?state{ cname = CName
                          , desired_state = #desired_state{ desired_audio_ssrc = SSRC }
                          } = State
                  ) ->

  PayloadTypeId = rtp_engine_passthrough_encoding_id(RTPEngineSnapshot),

  NewState =
    State#?state{ audio_stream_element_config =
                    #media_gateway_stream_element_config{ media_socket = Socket
                                                        , egest_crypto = EgestCrypto
                                                        , cname = CName
                                                        , payload_type_id = PayloadTypeId
                                                        , input_ssrc = SSRC
                                                        }
                },

  maybe_add_to_media_gateway(NewState);

handle_media_frame(#webrtc_media_channel_message{ message = #webrtc_media_channel_ready{ channel_type = video
                                                                                       , media_socket = Socket
                                                                                       , egest_crypto = EgestCrypto
                                                                                       , rtp_engine_snapshot = RTPEngineSnapshot
                                                                                       }
                                                },
                   #?state{ cname = CName
                          , desired_state = #desired_state{ desired_video_ssrc = SSRC }
                          } = State
                  ) ->

  PayloadTypeId = rtp_engine_passthrough_encoding_id(RTPEngineSnapshot),

  NewState =
    State#?state{ video_stream_element_config =
                    #media_gateway_stream_element_config{ media_socket = Socket
                                                        , egest_crypto = EgestCrypto
                                                        , cname = CName
                                                        , payload_type_id = PayloadTypeId
                                                        , input_ssrc = SSRC
                                                        }
                },

  maybe_add_to_media_gateway(NewState).

handle_info(#rtp_sequence{ type = audio } = Sequence, State) ->
  handle_audio_sequence(Sequence, State);

handle_info(#rtp_sequence{ type = video, rtps = Packets } = Sequence,
            #?state{ desired_state =
                       #desired_state{ desired_video_ssrc = DesiredVideoSSRC
                                     , desired_audio_ssrc = DesiredAudioSSRC
                                     , profile_name = ProfileName
                                     }
                   , video_state = VideoState
                   , audio_state = AudioState
                   , web_socket = WebSocket
                   } = State
           ) ->
  case lists:any(is_valid_switch_point(DesiredVideoSSRC), Packets) of
    true ->
      ?SLOG_INFO("Found valid switch point, switching SSRC", #{ video_ssrc => DesiredVideoSSRC }),
      rtsv2_player_ws_resource:notify_profile_switched(ProfileName, WebSocket),
      NewState = State#?state{ desired_state = undefined
                             , video_state = VideoState#egest_stream_state{ active_ssrc = DesiredVideoSSRC }
                             , audio_state = AudioState#egest_stream_state{ active_ssrc = DesiredAudioSSRC }
                             },
      handle_video_sequence(Sequence, NewState);
    false ->
      handle_video_sequence(Sequence, State)
  end;

handle_info(#rtp_sequence{ type = video } = Sequence, State) ->
  handle_video_sequence(Sequence, State).

handle_cast({set_active_profile, ProfileName}, State) ->
 case is_valid_profile_name(ProfileName, State) of
   true ->
     {noreply, set_active_profile_impl(State#?state{active_profile_name = ProfileName})};
   false ->
     {noreply, State}
 end;

handle_cast({set_active_profiles, ActiveProfileNames}, State = #?state{profiles = Profiles, valid_profiles = ValidProfiles}) ->
  ProfilesWithActiveFlag = [maps:put(isActive, lists:member(Name, ActiveProfileNames), P) || P = #{profileName := Name} <- Profiles],
  ValidProfilesWithActiveFlag = [maps:put(isActive, lists:member(Name, ActiveProfileNames), P) || P = #{profileName := Name} <- ValidProfiles],
  {noreply, set_active_profile_impl(State#?state{profiles = ProfilesWithActiveFlag,
                                                 valid_profiles = ValidProfilesWithActiveFlag})};

handle_cast({set_quality_constraint, force_quality, ProfileName}, State) ->
 case is_valid_profile_name(ProfileName, State) of
   true ->
     NewState1 =
       State#?state{ quality_constraint_behaviour = force_quality
                   , quality_constraint_profile = ProfileName
                   , active_profile_name = ProfileName
                   },

     NewState2 = set_active_profile_impl(NewState1),
     {noreply, NewState2};
   false ->
  {noreply, State}
  end;


handle_cast({set_quality_constraint, max_quality, MaxProfile}, #?state{ active_profile_name = ActiveProfileName, profiles = Profiles } = State) ->
 case is_valid_profile_name(MaxProfile, State) of
   true ->
     NewState1 =
       State#?state{ quality_constraint_behaviour = max_quality
                   , quality_constraint_profile = MaxProfile
                   },

     {ProfileAction, ValidProfiles} = decide_active_profile(ActiveProfileName, MaxProfile, Profiles),

     NewState2 = NewState1#?state{ valid_profiles = ValidProfiles },

     NewState3 =
       case ProfileAction of
         keep_active_profile ->
           NewState2;

         set_to_max_profile ->
           set_active_profile_impl(NewState2#?state{active_profile_name = MaxProfile})
       end,

     {noreply, NewState3};
   false ->
       {noreply, State}
 end;

handle_cast(notify_socket_disconnect, State) ->
  NewState = maybe_remove_from_media_gateway(State),
  {stop, normal, NewState}.

handle_audio_sequence(#rtp_sequence{ type = audio } = Sequence, #?state{ audio_state = StreamState } = State) ->
  case filter_and_renumber(Sequence, StreamState) of
    false ->
      { ok
      , State
      };

    {FilteredSequence, NewStreamState} ->
      { ok
      , FilteredSequence
      , State#?state{ audio_state = NewStreamState }
      }
  end.

handle_video_sequence(#rtp_sequence{ type = video } = Sequence, #?state{ video_state = StreamState } = State) ->
  case filter_and_renumber(Sequence, StreamState) of
    false ->
      { ok
      , State
      };

    {FilteredSequence, NewStreamState} ->
      { ok
      , FilteredSequence
      , State#?state{ video_state = NewStreamState }
      }
  end.


set_active_profile_impl(#?state{ active_profile_name = ProfileName
                               , profiles = Profiles } = State) ->

  ActualProfile = best_candidate(ProfileName, Profiles),
  case ActualProfile of
    undefined ->
      State;
    #{ profileName := ActualProfileName, firstAudioSSRC := AudioSSRC, firstVideoSSRC := VideoSSRC } ->
      apply_desired_state(
        #desired_state{ desired_video_ssrc = VideoSSRC
                      , desired_audio_ssrc = AudioSSRC
                      , profile_name = ProfileName
                      },
        State#?state{active_profile_name = ActualProfileName}
       );
    false->
      State
  end.


apply_desired_state(DesiredState = #desired_state{ desired_video_ssrc = VideoSSRC},
                    #?state{ audio_stream_element_config = ASE, video_stream_element_config = VSE } = State)
  when ASE =:= undefined orelse (VSE =:= undefined andalso VideoSSRC /= undefined) ->

  State#?state{ desired_state = DesiredState
              };

apply_desired_state( #desired_state{ desired_video_ssrc = VideoSSRC
                                   , desired_audio_ssrc = AudioSSRC
                                   },
                     #?state{ media_gateway_client_id = ClientId } = State
                   ) ->

  rtsv2_media_gateway_api:update_egest_client_subscription(ClientId, AudioSSRC, VideoSSRC),

  State.


filter_and_renumber(#rtp_sequence{ rtps = [] }, _StreamState) ->
  false;

filter_and_renumber(#rtp_sequence{ rtps = RTPs } = Sequence, State) ->
  case filter_and_renumber_prime(RTPs, [], State) of
    false ->
      false;

    { RTPsOut, NewState } ->
      { Sequence#rtp_sequence{ rtps = RTPsOut }, NewState }
  end.


filter_and_renumber_prime([], Acc, State) ->
  case Acc of
    [] ->
      false;

    _ ->
      { lists:reverse(Acc), State }
  end;

filter_and_renumber_prime([ #rtp{ ssrc = PacketSSRC } | Rest ], Acc, #egest_stream_state{ active_ssrc = ActiveSSRC } = State) when PacketSSRC =/= ActiveSSRC ->
  filter_and_renumber_prime(Rest, Acc, State);

filter_and_renumber_prime([ #rtp{ timestamp = RawTimestamp } = RTP | Rest ], Acc, #egest_stream_state{ last_sequence = LastSequence, last_timestamp = LastTimestamp, timestamp_delta = LastTimestampDelta, egest_ssrc = EgestSSRC } = State ) ->

  %% TODO: rollover...
  %% TODO: foward jump...
  {Timestamp, TimestampDelta} =
    case RawTimestamp + LastTimestampDelta of
      N when LastTimestamp =/= undefined andalso N < LastTimestamp ->
        NewDelta = LastTimestamp - RawTimestamp,
        ?SLOG_WARNING("Timestamps went back. Adjusting.", #{ from => LastTimestamp, to => N, previous_delta => LastTimestampDelta, new_delta => NewDelta }),
        {LastTimestamp, NewDelta};
      N ->
        {N, LastTimestampDelta}
    end,

  SequenceNumberOut = (LastSequence + 1) rem 65536,

  RTPOut = RTP#rtp{ ssrc = EgestSSRC
                  , sequence_number = SequenceNumberOut
                  , timestamp = Timestamp
                  },

  NewAcc = [ RTPOut | Acc ],

  NewState = State#egest_stream_state{ last_sequence = SequenceNumberOut
                                     , last_timestamp = Timestamp
                                     , timestamp_delta = TimestampDelta
                                     },

  filter_and_renumber_prime(Rest, NewAcc, NewState).


is_valid_switch_point(DesiredVideoSSRC) ->
  fun
    (#rtp{ ssrc = PacketSSRC }) when PacketSSRC =/= DesiredVideoSSRC ->
      false;

    (#rtp{ data = NALUs }) ->
      is_valid_switch_h264(NALUs)
  end.


%% TODO: not this
-include_lib("id3as_rtc/src/rtp/rtp_h264.hrl").

is_valid_switch_h264(<< ?FORBIDDEN_ZERO_BIT:1/integer, _NRI:2/integer, ?PACKET_TYPE_STAP_A:5/integer, UnitsBinary/binary>>) ->
  ?SLOG_INFO("Scanning STAP-A for SPS", #{}),
  NALUs = parse_single_time_aggregation_units_prime(UnitsBinary, []),
  lists:any(fun is_valid_switch_h264/1, NALUs);

is_valid_switch_h264(<< ?FORBIDDEN_ZERO_BIT:1/integer, _NRI:2/integer, ?PACKET_TYPE_SEQUENCE_PARAMETER_SET:5/integer, _Rest/binary>>) ->
  ?SLOG_INFO("Found SPS", #{}),
  true;

is_valid_switch_h264(<< ?FORBIDDEN_ZERO_BIT:1/integer, _NRI:2/integer, _Type:5/integer, _Rest/binary>>) ->
  false.


%% HACK, copied from h264_ingest
parse_single_time_aggregation_units_prime(<<>>, Results) ->
  lists:reverse(Results);

parse_single_time_aggregation_units_prime(<<UnitSize:16/integer, Unit:UnitSize/binary, Remainder/binary>>, Results) ->
  parse_single_time_aggregation_units_prime(Remainder, [Unit | Results]).


maybe_add_to_media_gateway(#?state{ audio_stream_element_config = undefined } = State) ->
  {ok, State};

maybe_add_to_media_gateway(#?state{ video_stream_element_config = undefined, audio_only = false } = State) ->
  {ok, State};

maybe_add_to_media_gateway(#?state{ media_gateway_client_id = ClientId
                                  , slot_id = <<SlotId:128/big-integer>>
                                  , slot_role = { SlotRole }
                                  , audio_stream_element_config = AudioStreamElementConfig
                                  , video_stream_element_config = VideoStreamElementConfig
                                  } = State) ->

  Config =
    #media_gateway_egest_client_config{ audio = AudioStreamElementConfig
                                      , video = VideoStreamElementConfig
                                      },

  rtsv2_media_gateway_api:add_egest_client(SlotId, SlotRole, ClientId, Config),

  {ok, State}.


maybe_remove_from_media_gateway(#?state{ audio_stream_element_config = undefined } = State) ->
  {ok, State};

maybe_remove_from_media_gateway(#?state{ video_stream_element_config = undefined } = State) ->
  {ok, State};

maybe_remove_from_media_gateway(#?state{ media_gateway_client_id = ClientId
                                       } = State) ->
  rtsv2_media_gateway_api:remove_egest_client(ClientId),
  State.


rtp_engine_passthrough_encoding_id(RTPEngine) ->
  [ { _, { rtp_passthrough_egest, EgestHandlerState} } ] = maps:to_list(rtp_engine:egest_handlers(RTPEngine)),
  rtp_passthrough_egest:encoding_id(EgestHandlerState).


decide_active_profile(_ActiveProfileName, MaxProfileName, [#{ profileName := HeadProfileName } | _Tail] = ValidProfiles) when MaxProfileName =:= HeadProfileName ->

  %% If we reached the max profile before the active profile, and given
  %% that the profiles are in order from highest bitrate to lowest, that
  %% means the max profile has a higher bitrate (or equal) bitrate to the
  %% active profile, and the active profile can remain unchanged
  {keep_active_profile, ValidProfiles};

decide_active_profile(ActiveProfileName, MaxProfileName, [#{ profileName := HeadProfileName } | Tail]) when ActiveProfileName =:= HeadProfileName ->

  %% If we reached the active profile before the max profile, and given
  %% that the profiles are in order from highest bitrate to lowest, that
  %% means the active profile is too high of a bitrate
  {set_to_max_profile, lists:dropwhile(fun(#{ profileName := RemainingProfileName }) -> RemainingProfileName =/= MaxProfileName end, Tail)};

decide_active_profile(ActiveProfile, MaxProfile, [_Head | Tail ]) ->
  decide_active_profile(ActiveProfile, MaxProfile, Tail).

choose_profile_for_bandwidth(_Bandwidth, [ #{ profileName := ProfileName } ]) ->
  ProfileName;
choose_profile_for_bandwidth(Bandwidth, [ #{ bitrate := Bitrate, profileName := ProfileName } | _Tail ]) when Bitrate =< Bandwidth ->
  ProfileName;
choose_profile_for_bandwidth(Bandwidth, [ _Head | Tail ]) ->
  choose_profile_for_bandwidth(Bandwidth, Tail).

is_valid_profile_name(ProfileName, #?state{profiles = Profiles}) ->
  lists:any(fun(#{ profileName := ActualProfileName }) -> ActualProfileName =:= ProfileName end, Profiles).

best_candidate(ProfileName, Profiles) ->
  best_candidate(undefined, ProfileName, Profiles).

best_candidate(BestHigher, _ProfileName, []) ->
  BestHigher;
best_candidate(_BestHigher, ProfileName,  [ExactMatch = #{ profileName := ProfileName
                                                         , isActive := true
                                                         } | _]) ->
  ExactMatch;
best_candidate(BestHigher, ProfileName,  [#{ profileName := ProfileName
                                           , isActive := false
                                           } | T]) ->
  best_candidate_lower(BestHigher, T);
best_candidate(_BestHigher, ProfileName,  [NewBestHigher = #{ isActive := true
                                                           } | T]) ->
  best_candidate(NewBestHigher, ProfileName, T);
best_candidate(BestHigher, ProfileName,  [#{ isActive := false
                                           } | T]) ->
  best_candidate(BestHigher, ProfileName, T).

best_candidate_lower(BestHigher, []) ->
  BestHigher;
best_candidate_lower(_BestHigher, [BestLower = #{ isActive := true
                                                } | _]) ->
  BestLower;
best_candidate_lower(BestHigher, [#{ isActive := false
                                   } | T]) ->
  best_candidate_lower(BestHigher, T).
