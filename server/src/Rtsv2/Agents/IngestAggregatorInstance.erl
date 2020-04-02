-module(rtsv2_agents_ingestAggregatorInstance@foreign).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/transcode.hrl").
-include_lib("id3as_media/include/audio_levels.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").
-include_lib("id3as_media/include/fun_processor.hrl").
-include_lib("id3as_media/include/rtmp.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").
-ifdef(__RTC_FEAT_FEC).
-include_lib("id3as_rtc/include/rtp_ulp_fec.hrl").
-endif.
-include("../../../../src/rtsv2_slot_media_source_publish_processor.hrl").
-include("../../../../src/rtsv2_rtp.hrl").

-export([
         startWorkflowImpl/3,
         stopWorkflowImpl/1,
         addLocalIngestImpl/2,
         addRemoteIngestImpl/3,
         removeIngestImpl/2,
         registerStreamRelayImpl/3,
         deRegisterStreamRelayImpl/3,
         workflowMessageMapperImpl/1
        ]).

-define(frames_with_source_id(SourceId), #named_ets_spec{name = list_to_atom("frames_with_source_id: " ++ ??SourceId),
                                                         spec = ?wildcard_by_name(frame)#frame{source_metadata = ?wildcard(#source_metadata{})#source_metadata{source_id = SourceId}}}).


-record(enriched_slot_profile,
        { ingest_key :: term()
        , stream_name :: binary_string()
        , profile_name :: binary_string()
        , audio_ssrc_start :: non_neg_integer()
        , video_ssrc_start :: non_neg_integer()
        }).


startWorkflowImpl(SlotId, SlotRole, ProfileArray) ->
  fun() ->
      Profiles = array:to_list(ProfileArray),

      {EnrichedProfiles, _NextProfileIndex} =
        lists:mapfoldl(fun({ IngestKey, StreamName, ProfileName }, ProfileIndex) ->
                           ProfileInfo =
                             #enriched_slot_profile{ ingest_key = IngestKey
                                                   , stream_name = StreamName
                                                   , profile_name = ProfileName

                                                     %% SSRCs are 32-bits, use the upper 16 bits to reflect
                                                     %% the profile to which the RTP stream belongs, the
                                                     %% next 8 bits to provide a type, and leave the lower
                                                     %% 8 bits incase we ever need multiple related SSRCs
                                                     %% for a single RTP flow
                                                   , audio_ssrc_start = ?make_audio_ssrc(ProfileIndex, 0)
                                                   , video_ssrc_start = ?make_video_ssrc(ProfileIndex, 0)
                                                   },
                           { ProfileInfo, ProfileIndex + 1 }
                       end,
                       ?PROFILE_INDEX_STANDARD_OFFSET,
                       Profiles
                      ),

        startWorkflow(SlotId, SlotRole, EnrichedProfiles)
  end.

stopWorkflowImpl(Handle) ->
  fun() ->
      ok = id3as_workflow:stop(Handle)
  end.

addLocalIngestImpl(Handle, IngestKey) ->
  fun() ->
      ok = id3as_workflow:ioctl(ingests, {add_local_ingest, IngestKey}, Handle)
  end.

addRemoteIngestImpl(Handle, IngestKey, Url) ->
  fun() ->
      ok = id3as_workflow:ioctl(ingests, {add_remote_ingest, IngestKey, Url}, Handle)
  end.

removeIngestImpl(Handle, IngestKey) ->
  fun() ->
      ok = id3as_workflow:ioctl(ingests, {remove_ingest, IngestKey}, Handle)
  end.

registerStreamRelayImpl(Handle, Host, Port) ->
  fun() ->
      io:format(user, "Going to start sending information to ~p:~p", [Host, Port]),
      ok = id3as_workflow:ioctl(slot_media_source_publish, {register_stream_relay, Host, Port}, Handle),
      ok
  end.

deRegisterStreamRelayImpl(Handle, Host, Port) ->
  fun() ->
      io:format(user, "Going to stop sending information to ~p:~p", [Host, Port]),
      ok = id3as_workflow:ioctl(slot_media_source_publish, {deregister_stream_relay, Host, Port}, Handle),
      ok
  end.

workflowMessageMapperImpl(#workflow_output{message = #workflow_data_msg{data = #frame{type = script,
                                                                                      pts = Pts,
                                                                                      frame_metadata = #rtmp_onfi_timestamp{
                                                                                                          timestamp = Timestamp
                                                                                                         }}}}) ->
  {just, {rtmpOnFI, Timestamp, Pts}};

workflowMessageMapperImpl(#workflow_output{}) ->
  {just, {noop}};

workflowMessageMapperImpl(_) ->
  {nothing}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
startWorkflow(SlotId, SlotRole, Profiles) ->

  SlotConfiguration = slot_configuration(SlotId, Profiles),

  Pids = lists:map(fun(IngestKey) ->
                       {ok, Pid} = webrtc_stream_server:start_link(IngestKey, #{stream_module => rtsv2_webrtc_ingest_preview_stream_handler,
                                                                                stream_module_args => [IngestKey]}),
                       Pid
                   end,
                   [ IngestKey || #enriched_slot_profile{ ingest_key = IngestKey } <- Profiles ]),

  AudioConfig = #audio_transcode_config{
                   input = #frame_spec{
                              profile = #audio_profile{}
                             },
                   outputs = [
                              #named_output{
                                 profile_name = opus,
                                 frame_spec = #frame_spec{
                                                 profile = #audio_profile{
                                                              codec = opus,
                                                              sample_rate = 48000,
                                                              sample_format = s16
                                                             },
                                                 encoder_parameters = #libopus_encoder_config{
                                                                         bitrate = 96000,
                                                                         frame_size_ms = 20
                                                                        }
                                                }
                                }
                             ]
                  },

  Workflow = #workflow{
                name = {ingest_aggregator_instance, SlotId},
                display_name = <<"RTMP Ingest Aggregator">>,
                tags = #{type => ingest_aggregator,
                         slot => SlotId},
                generators = [
                              #generator{name = ingests,
                                         display_name = <<"Ingests">>,
                                         module = rtsv2_rtmp_ingest_generator
                                        }
                             ],

                processors = [
                              #processor{name = aggregate,
                                         display_name = <<"Gather Ingests">>,
                                         subscribes_to = ingests,
                                         module = rtsv2_rtmp_ingest_aggregator_processor
                                        },

                              lists:map(fun(#enriched_slot_profile{ ingest_key = IngestKey
                                                                  , stream_name = StreamName
                                                                  , profile_name = ProfileName
                                                                  , audio_ssrc_start = AudioSSRCStart
                                                                  , video_ssrc_start = VideoSSRCStart
                                                                  }
                                           ) ->

                                            #compound_processor{
                                               name = binary_to_atom(ProfileName, utf8),
                                               display_name = <<ProfileName/binary, " (receiving from ", StreamName/binary, ")">>,
                                               spec = #processor_spec{consumes = [?audio_frames, ?video_frames]},
                                               subscribes_to = [{aggregate, ?audio_frames_with_source_id(ProfileName)},
                                                                {aggregate, ?video_frames_with_source_id(ProfileName)}],
                                               processors = [
                                                             #processor{name = audio_decode,
                                                                        display_name = <<"Audio Decode">>,
                                                                        subscribes_to = {outside_world, ?audio_frames},
                                                                        module = fdk_aac_decoder
                                                                       },

                                                             #processor{name = audio_levels,
                                                                        display_name = <<"dB Levels">>,
                                                                        subscribes_to = ?previous,
                                                                        module = audio_levels,
                                                                        config = #audio_levels_config{mode = consumes,
                                                                                                      tag = levels}
                                                                       },

                                                             #processor{name = levels_to_bus,
                                                                        display_name = <<"Publish Levels">>,
                                                                        subscribes_to = ?previous,
                                                                        module = send_to_bus_processor,
                                                                        config = #send_to_bus_processor_config{consumes = true,
                                                                                                               bus_name = {audio_levels, IngestKey}}
                                                                       },

                                                             #processor{name = audio_transcode,
                                                                        display_name = <<"Audio Transcode">>,
                                                                        subscribes_to = audio_decode,
                                                                        module = audio_transcode,
                                                                        config = AudioConfig
                                                                       },

                                                             #processor{name = rtp,
                                                                        display_name = <<"WebRTC RTP Mux">>,
                                                                        module = fun_processor,
                                                                        subscribes_to = [?previous, {outside_world, ?video_frames}],
                                                                        config = #fun_processor_config{ initial_state =
                                                                                                          { rtp_opus_egest:new(AudioSSRCStart, ?OPUS_ENCODING_ID)
                                                                                                          , rtp_h264_egest:new(VideoSSRCStart, ?H264_ENCODING_ID)
                                                                                                          }
                                                                                                      , function = fun mux_to_rtp/2
                                                                                                      , spec = #processor_spec{ consumes = ?frames,
                                                                                                                                generates = ?rtp_sequences
                                                                                                                              }
                                                                                                      }
                                                                       },

                                                             #processor{
                                                                name = fec,
                                                                display_name = <<"WebRTC FEC">>,
                                                                module = passthrough_processor, %%fun_processor,
                                                                subscribes_to = ?previous,
                                                                config = #fun_processor_config{ initial_state = 1
                                                                                              , function = fun webrtc_fec/2
                                                                                              , spec = #processor_spec{ consumes = ?rtp_sequences,
                                                                                                                        generates = ?rtp_sequences }
                                                                                              }
                                                               },

                                                             #processor{
                                                                name = baked_rtp,
                                                                display_name = <<"Baked RTP">>,
                                                                module = passthrough_processor,
                                                                subscribes_to = ?previous
                                                               },

                                                             #processor{
                                                                name = webrtc_bus_publish,
                                                                display_name = <<"WebRTC Publish">>,
                                                                subscribes_to = [baked_rtp, {outside_world, ?program_details_frames}],
                                                                module = send_to_bus_processor,
                                                                config = #send_to_bus_processor_config{
                                                                            consumes = true,
                                                                            bus_name = {webrtc_stream_output, IngestKey}
                                                                           }
                                                               },

                                                             #processor{
                                                                name = baked_rtp_emit,
                                                                display_name = <<"Emit Baked RTP for Source">>,
                                                                module = passthrough_processor,
                                                                subscribes_to = baked_rtp
                                                               }

                                                             %% #processor{
                                                             %%    name = timestamp_logger,
                                                             %%    subscribes_to = outside_world,
                                                             %%    module = fun_processor,
                                                             %%    config = #fun_processor_config{ initial_state = ?now_us
                                                             %%                                  , function = fun(#frame{type = Type}, Last) ->
                                                             %%                                                   Now = ?now_us,
                                                             %%                                                   Stat = {'rtp', [{delta, Now - Last}], [{type, Type}]},
                                                             %%                                                   %%vmstats_influx_sink:collect([Stat]),
                                                             %%                                                   ?SLOG_DEBUG("rtp", #{stat => Stat}),
                                                             %%                                                   {ok, Now}
                                                             %%                                               end
                                                             %%                                  , spec = #processor_spec{ consumes = ?all }
                                                             %%                                  }
                                                             %% }

                                                            ]
                                              }
                                        end,
                                        Profiles
                                       ),

                              #processor{ name = slot_media_source_publish
                                        , subscribes_to = [ binary_to_atom(ProfileName, utf8) || #enriched_slot_profile{ profile_name = ProfileName } <- Profiles ]
                                        , module = rtsv2_slot_media_source_publish_processor
                                        , config =
                                            #rtsv2_slot_media_source_publish_processor_config{ slot_name = SlotId
                                                                                             , slot_role = SlotRole
                                                                                             , slot_configuration = SlotConfiguration
                                                                                             }
                                        }
                             ]
               },

  {ok, Pid} = id3as_workflow:start_link(Workflow, self()),

  {ok, Handle} = id3as_workflow:workflow_handle(Pid),

  {Handle, Pids, SlotConfiguration}.

mux_to_rtp(#frame{ profile = #audio_profile{ codec = Codec } } = Frame, { AudioEgest, VideoEgest }) ->
  { NewAudioEgest, RTPs } = rtp_opus_egest:step(AudioEgest, Frame),

  { #rtp_sequence{ type = audio, codec = Codec, rtps = RTPs }
  , { NewAudioEgest, VideoEgest }
  };

mux_to_rtp(#frame{ profile = #video_profile{ codec = Codec } } = Frame, { AudioEgest, VideoEgest }) ->
  { NewVideoEgest, RTPs } = rtp_h264_egest:step(VideoEgest, Frame),

  { #rtp_sequence{ type = video, codec = Codec, rtps = RTPs }
  , { AudioEgest, NewVideoEgest }
  };

mux_to_rtp(_, State) ->
  {ok, State}.

webrtc_fec(#rtp_sequence{codec = h264,
                         rtps = Rtps}, NextSequenceNumber) ->

  {MediaRtps, FecRtps, NextSequenceNumber2} = do_webrtc_fec(Rtps, NextSequenceNumber),

  {[#rtp_sequence{type = video,
                  codec = h264,
                  rtps = MediaRtps},
    #rtp_sequence{type = video,
                  codec = ulpfec,
                  rtps = FecRtps}], NextSequenceNumber2};

webrtc_fec(Sequence, State) ->
  {Sequence, State}.

update_seq_number(Rtp, NextSequenceNumber) ->
  {Rtp#rtp{sequence_number = NextSequenceNumber}, NextSequenceNumber + 1}.

do_webrtc_fec(MediaRtps, NextSequenceNumber) ->
  do_webrtc_fec(MediaRtps, NextSequenceNumber, [], [], []).

do_webrtc_fec([], NextSequenceNumber, [], MediaAcc, FecAcc) ->
  %% Here we need to set the seq number of the fec packets...
  NumFecs = length(FecAcc),

  {_, FecAcc2} =
    lists:foldl(fun(Fec, {SequenceNumber, Acc}) ->
                    Fec2 = Fec#rtp{sequence_number = SequenceNumber},
                    {SequenceNumber - 1, [Fec2 | Acc]}
                end,
                {NextSequenceNumber + NumFecs - 1, []},
                FecAcc),

  {lists:reverse(MediaAcc), FecAcc2, NextSequenceNumber + NumFecs};

do_webrtc_fec([], NextSequenceNumber, PendingPackets, MediaAcc, FecAcc) ->
  %% We have run out of input but still have some media to protect
  FecAcc2 = make_fec(lists:reverse(PendingPackets), NextSequenceNumber - 1, FecAcc),
  do_webrtc_fec([], NextSequenceNumber, [], MediaAcc, FecAcc2);

do_webrtc_fec([Media | MediaT], NextSequenceNumber, PendingPackets, MediaAcc, FecAcc) ->
  {Media2, NextSequenceNumber2} = update_seq_number(Media, NextSequenceNumber),

  PendingPackets2 = [Media2 | PendingPackets],
  case length(PendingPackets2) of
    N when N >= 10 ->
      %% We have enough to protect
      FecAcc2 = make_fec(lists:reverse(PendingPackets2), NextSequenceNumber, FecAcc),
      case rand:uniform() of
        Drop when Drop < 0 ->
          do_webrtc_fec(MediaT, NextSequenceNumber2, [], MediaAcc, FecAcc2);
        _ ->
          do_webrtc_fec(MediaT, NextSequenceNumber2, [], [Media2 | MediaAcc], FecAcc2)
      end;
    _ ->
      do_webrtc_fec(MediaT, NextSequenceNumber2, PendingPackets2, [Media2 | MediaAcc], FecAcc)
  end.

-ifdef(__RTC_FEAT_FEC).
make_fec(MediaRtps = [First = #rtp{sequence_number = FirstSeqNumber} | _], LastSeqNumber, FecAcc) ->

  FECEncodingId = 116,
  Length = lists:foldl(fun(#rtp{data = Data}, Max) ->
                           max(byte_size(Data), Max)
                       end,
                       0,
                       MediaRtps),

  Fec = rtp_ulp_fec:generate_fec(MediaRtps,
                                 [#ulp_level{
                                     packets_protected = lists:seq(FirstSeqNumber, LastSeqNumber),
                                     protection_length = Length
                                    }]),

  FecRtp = First#rtp{
             payload_type = #rtp_payload_type{encoding_id = FECEncodingId, clock_rate = 90000},
             data = rtp_ulp_fec:unparse_fec(Fec),
             marker = 0
            },

  [FecRtp | FecAcc].
-else.
make_fec(_MediaRtps, _LastSeqNumber, FecAcc) ->
  FecAcc.
-endif.


-spec slot_configuration(binary_string(), list(#enriched_slot_profile{})) -> rtsv2_slot_configuration:configuration().
slot_configuration(SlotId, Profiles) ->
  #{ profiles => [ profile(Profile) || Profile <- Profiles ]
   , name => SlotId
   }.


profile(#enriched_slot_profile{ profile_name = ProfileName, audio_ssrc_start = AudioSSRCStart, video_ssrc_start = VideoSSRCStart }) ->
  #{ name => ProfileName
   , firstAudioSSRC => AudioSSRCStart
   , firstVideoSSRC => VideoSSRCStart
   }.
