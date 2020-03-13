-module(rtsv2_webrtc_session_handler).

-behaviour(webrtc_session_handler).

-export([ set_active_profile/3
        ]).

-export([ init/1
        , handle_media_frame/2
        , handle_info/2
        , handle_cast/2
        ]).

-include_lib("id3as_rtc/include/rtp.hrl").
-include_lib("id3as_rtc/include/rtp_engine.hrl").
-include("./src/rtsv2_rtp.hrl").

-define(state, ?MODULE).

%% TODO: review sequence number behaviour
-record(?state,
        { session_id
        , profiles

        , active_video_ssrc
        , desired_video_ssrc
        , last_video_sequence = 16

        , active_audio_ssrc
        , desired_audio_ssrc
        , last_audio_sequence = 16
        }).

%%% timestamps?????? lip sync

set_active_profile(ServerId, TraceId, ProfileName) ->
  webrtc_stream_server:cast_session(ServerId, TraceId, {set_active_profile, ProfileName}).


init([ SessionId, [ #{ name := ActiveProfileName } | _ ] = Profiles ]) ->
  ?DEBUG("Session handler started for session ~p in profile ~p", [SessionId, ActiveProfileName]),
  State1 = #?state{ session_id = SessionId, profiles = Profiles },
  State2 = set_active_profile_impl(ActiveProfileName, State1),
  State2.

handle_media_frame(#rtp_engine_msg{message = _RTPEngineState}, State) ->
  %% NOTE: we don't care whether the engine is ready or not, we're not receiving
  %%       media, and it will drop media we're sending if it isn't ready
  {ok, State};

handle_media_frame(_Frame, #?state{} = State) ->
  {ok, State}.

handle_info(#rtp_sequence{ type = audio } = Sequence, State) ->
  handle_audio_sequence(Sequence, State);

handle_info(#rtp_sequence{ type = video, rtps = Packets } = Sequence, #?state{ desired_video_ssrc = DesiredVideoSSRC, desired_audio_ssrc = DesiredAudioSSRC } = State) when DesiredVideoSSRC =/= undefined ->
  case lists:any(is_valid_switch_point(DesiredVideoSSRC), Packets) of
    true ->
      ?SLOG_INFO("Found valid switch point, switching SSRC", #{ video_ssrc => DesiredVideoSSRC }),
      handle_video_sequence(Sequence, State#?state{ desired_video_ssrc = undefined, desired_audio_ssrc = undefined, active_video_ssrc = DesiredVideoSSRC, active_audio_ssrc = DesiredAudioSSRC });
    false ->
      handle_video_sequence(Sequence, State)
  end;

handle_info(#rtp_sequence{ type = video } = Sequence, State) ->
  handle_video_sequence(Sequence, State).

handle_cast({set_active_profile, Profile}, State) ->
  NewState = set_active_profile_impl(Profile, State),
  {noreply, NewState};

handle_cast(notify_socket_disconnect, State) ->
  {stop, normal, State}.

handle_audio_sequence(#rtp_sequence{ type = audio } = Sequence, #?state{ active_audio_ssrc = ActiveSSRC, last_audio_sequence = LastSequenceNumber } = State) ->
  case filter_and_renumber(Sequence, ActiveSSRC, LastSequenceNumber, ?EGEST_AUDIO_SSRC) of
    false ->
      { ok
      , State
      };
    {FilteredSequence, NewLastSequenceNumber} ->
      { ok
      , FilteredSequence
      , State#?state{ last_audio_sequence = NewLastSequenceNumber }
      }
  end.

handle_video_sequence(#rtp_sequence{ type = video } = Sequence, #?state{ active_video_ssrc = ActiveSSRC, last_video_sequence = LastSequenceNumber } = State) ->
  case filter_and_renumber(Sequence, ActiveSSRC, LastSequenceNumber, ?EGEST_VIDEO_SSRC) of
    false ->
      { ok
      , State
      };
    {FilteredSequence, NewLastSequenceNumber} ->
      { ok
      , FilteredSequence
      , State#?state{ last_video_sequence = NewLastSequenceNumber }
      }
  end.

set_active_profile_impl(ProfileName, #?state{ profiles = Profiles } = State) ->
  MaybeMatchingProfile = lists:search(fun(#{ name := ActualProfileName }) -> ActualProfileName =:= ProfileName end, Profiles),

  case MaybeMatchingProfile of
    {value, #{ firstAudioSSRC := AudioSSRC, firstVideoSSRC := VideoSSRC }} ->
      State#?state{ desired_video_ssrc = VideoSSRC
                  , desired_audio_ssrc = AudioSSRC
                  };
    false->
      State
  end.

filter_and_renumber(#rtp_sequence{ rtps = RTPs } = Sequence, ActiveSSRC, LastSequenceNumber, EgestSSRC) ->
  {NewRTPs, NewLastSequenceNumber} =
    lists:foldl(fun
                  (#rtp{ ssrc = PacketSSRC } = RTP, {NewRTPsIn, LastSequenceNumberIn}) when PacketSSRC =:= ActiveSSRC ->
                    SequenceNumberOut = (LastSequenceNumberIn + 1) rem 65536,
                    RTPOut = RTP#rtp{ ssrc = EgestSSRC, sequence_number = SequenceNumberOut },
                    NewRTPsOut = [ RTPOut | NewRTPsIn ],
                    { NewRTPsOut, SequenceNumberOut };

                  (_RTP, Acc) ->
                   Acc
                end,
                {[], LastSequenceNumber},
                RTPs
               ),

  case NewRTPs of
    [] ->
      false;
    _ ->
      {Sequence#rtp_sequence{ rtps = lists:reverse(NewRTPs) }, NewLastSequenceNumber}
  end.

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
