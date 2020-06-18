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

-include_lib("id3as_media/include/ts.hrl").
-include_lib("id3as_media/include/ts_encoder.hrl").
-include_lib("id3as_media/include/ts_segment_generator.hrl").

-include_lib("id3as_media/include/m3u8.hrl").

-include_lib("id3as_media/include/stream_sync.hrl").


-ifdef(__RTC_FEAT_FEC).
-include_lib("id3as_rtc/include/rtp_ulp_fec.hrl").
-endif.
-include("../../../../src/rtsv2_slot_media_source_publish_processor.hrl").
-include("../../../../src/rtsv2_rtp.hrl").
-include("../../../../src/gop_measurer.hrl").
-include("../../../../src/rtsv2_hls_segment_workflow.hrl").

-include("../../../../src/rtsv2_master_playlist_processor.hrl").

-export([
         startWorkflowImpl/2,
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


-define(program_details_frames_with_source_id(SourceId), #named_ets_spec{name = program_details_frames,
                                                                         spec = ?wildcard_by_name(frame)#frame{type = program_details,
                                                                                                              source_metadata = ?wildcard(#source_metadata{})#source_metadata{source_id = SourceId}
                                                                         }}).

startWorkflowImpl(SlotConfiguration, PushDetails) ->
  fun() ->
      startWorkflow(SlotConfiguration, PushDetails)
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

workflowMessageMapperImpl(#workflow_output{message = #workflow_data_msg{data = #frame{type = script_frame,
                                                                                      pts = Pts,
                                                                                      frame_metadata = #rtmp_onfi_message{
                                                                                                          amf = Amf
                                                                                                         }}}}) ->
  Json = amf:to_json(Amf),

  {just, {rtmpOnFI, Json, Pts}};

workflowMessageMapperImpl(#workflow_output{}) ->
  {just, {noop}};

workflowMessageMapperImpl(_) ->
  {nothing}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
startWorkflow(SlotConfiguration = #{ slotId := SlotId
                                   , slotRole := SlotRole
                                   , profiles := Profiles
                                   , audioOnly := AudioOnly}, PushDetails) ->

  Pids = lists:map(fun(IngestKey) ->
                       {ok, Pid} = webrtc_stream_server:start_link(IngestKey, #{stream_module => rtsv2_webrtc_ingest_preview_stream_handler,
                                                                                stream_module_args => [IngestKey]}),
                       Pid
                   end,
                   [ IngestKey || #{ ingestKey := IngestKey } <- Profiles ]),

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
                              |
                              ?include_if(PushDetails /= [],
                                          [
                                           #named_output{
                                              profile_name = aac,
                                              frame_spec = #frame_spec{
                                                              profile = #audio_profile{
                                                                           codec = aac,
                                                                           sample_rate = 48000,
                                                                           sample_format = s16
                                                                          }
                                                             }
                                             }
                                          )
                                         ]
                             ]
                  },

  Workflow = #workflow{
                name = {ingest_aggregator_instance, SlotId},
                display_name = <<"Ingest Aggregator">>,
                tags = #{type => ingest_aggregator,
                         slot => SlotId},
                generators = [
                              #generator{name = ingests,
                                         display_name = <<"Ingests">>,
                                         module = rtsv2_ingest_generator
                                        }
                             ],

                processors = [
                              #processor{name = aggregate,
                                         display_name = <<"Gather Ingests">>,
                                         subscribes_to = ingests,
                                         module = rtsv2_ingest_aggregator_processor,
                                         config = AudioOnly
                                        },

                              #processor{name = set_video_profile_name,
                                         display_name = <<"Set Video Profile Name">>,
                                         subscribes_to = {?previous, ?video_frames},
                                         module = fun_processor,
                                         config = #fun_processor_config{
                                                                          function = fun(Frame = #frame{source_metadata = #source_metadata{source_id = ProfileName},
                                                                                                        profile = Profile = #video_profile{}}, State) ->
                                                                                           {Frame#frame{ profile = Profile#video_profile{ name = ProfileName }}, State};
                                                                                        (Frame, State) -> {Frame, State}
                                                                                     end,
                                                                          spec = #processor_spec{ consumes = ?all }
                                                                       }
                                        },

                              #compound_processor{
                                               name = audio_transcode,
                                               display_name = <<"Audio Transcode">>,
                                               spec = #processor_spec{consumes = [?audio_frames]},
                                               subscribes_to = {aggregate, ?audio_frames},
                                               processors = [
                                                  #processor{name = binary_to_atom(ProfileName, utf8),
                                                             display_name = <<ProfileName/binary, " (receiving from ", StreamName/binary, ")">>,
                                                             subscribes_to = {outside_world, ?audio_frames_with_source_id(ProfileName)},
                                                             module = audio_transcode,
                                                             config = AudioConfig
                                                            }
                                                  || #{ streamName := StreamName
                                                      , profileName := ProfileName} <- Profiles
                                               ]

                                              },

                              #processor{name = gop_numberer,
                                         display_name = <<"GoP Numberer">>,
                                         subscribes_to = [set_video_profile_name, audio_transcode, {aggregate, ?program_details_frames}],
                                         module = gop_numberer
                                        },

                              #processor{name = gop_measurer,
                                         subscribes_to = [{?previous, ?audio_frames}, {?previous, ?video_frames}],
                                         module = gop_measurer
                                        },

                              #processor{name = log_pd, subscribes_to = {ingests, ?program_details_frames}, module = dev_null_processor, trace_inputs= console},

                              ?include_if(PushDetails /= [],
                                #processor{name = master_hls_playlists,
                                          display_name = <<"Publish Master HLS Playlists">>,
                                          subscribes_to = [
                                              {gop_numberer, ?audio_frames_with_profile_name(aac)},
                                              {gop_numberer, ?video_frames}
                                          ],
                                          module = rtsv2_hls_master_playlist_processor,
                                          config = #hls_master_playlist_processor_config{
                                              slot_id = SlotId,
                                              profiles = Profiles,
                                              push_details = PushDetails,
                                              audio_only = AudioOnly
                                            }
                                          }
                              ),

                              lists:map(fun(Profile = #{ ingestKey := IngestKey
                                                       , streamName := StreamName
                                                       , profileName := ProfileName
                                                       , firstAudioSSRC := AudioSSRCStart
                                                       , firstVideoSSRC := VideoSSRCStart
                                                       }
                                           ) ->

                                            #compound_processor{
                                               name = binary_to_atom(ProfileName, utf8),
                                               display_name = <<ProfileName/binary, " (receiving from ", StreamName/binary, ")">>,
                                               spec = #processor_spec{consumes = [?audio_frames, ?video_frames]},
                                               subscribes_to = [{gop_numberer, ?audio_frames_with_source_id(ProfileName)},
                                                                {gop_numberer, ?video_frames_with_source_id(ProfileName)},
                                                                {ingests, ?program_details_frames_with_source_id(ProfileName)}]
                                                                ++ case PushDetails of [] -> [];
                                                                                       _  -> [ {gop_measurer, ?gop_measurements_with_sourceid(ProfileName)} ]
                                                                   end,

                                               processors = [
                                                             ?include_if(PushDetails /= [],
                                                                         [
                                                                          #processor{name = hls_publish,
                                                                                     display_name = <<"HLS Publish">>,
                                                                                     subscribes_to = [
                                                                                                      {outside_world, ?video_frames},
                                                                                                      {outside_world, ?audio_frames_with_profile_name(aac)},
                                                                                                      {outside_world, ?gop_measurements},
                                                                                                      {outside_world, ?program_details_frames}
                                                                                                     ],
                                                                                     module = rtsv2_hls_segment_workflow,
                                                                                     config = #rtsv2_hls_segment_workflow_config{
                                                                                                 slot_id = SlotId,
                                                                                                 slot_profile = Profile,
                                                                                                 push_details = PushDetails,
                                                                                                 audio_only = AudioOnly
                                                                                                }
                                                                                    }
                                                                         ]),

                                                             #processor{name = rtp,
                                                                        display_name = <<"WebRTC RTP Mux">>,
                                                                        module = fun_processor,
                                                                        subscribes_to = [{outside_world, ?audio_frames_with_profile_name(opus)}, {outside_world, ?video_frames}],
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
                                        , subscribes_to = [ binary_to_atom(ProfileName, utf8) || #{ profileName := ProfileName } <- Profiles ]
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

  {Handle, Pids}.


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
