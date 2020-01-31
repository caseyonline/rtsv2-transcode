-module(rtsv2_agents_ingestAggregatorInstance@foreign).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/transcode.hrl").
-include_lib("id3as_media/include/audio_levels.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").
-include_lib("id3as_media/include/fun_processor.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").
-ifdef(__RTC_FEAT_FEC).
-include_lib("id3as_rtc/include/rtp_ulp_fec.hrl").
-endif.

-export([
         startWorkflowImpl/2,
         addLocalVariantImpl/2,
         addRemoteVariantImpl/3,
         removeVariantImpl/2
        ]).

-define(frames_with_source_id(SourceId), #named_ets_spec{name = list_to_atom("frames_with_source_id: " ++ ??SourceId),
                                                         spec = ?wildcard_by_name(frame)#frame{source_metadata = ?wildcard(#source_metadata{})#source_metadata{source_id = SourceId}}}).

startWorkflowImpl(SlotName, StreamNames) ->
  fun() ->
      startWorkflow(SlotName, array:to_list(StreamNames))
  end.

addLocalVariantImpl(Handle, StreamAndVariant) ->
  fun() ->
      ok = id3as_workflow:ioctl(ingests, {add_local_ingest, StreamAndVariant}, Handle)
  end.

addRemoteVariantImpl(Handle, StreamAndVariant, Url) ->
  fun() ->
      ok = id3as_workflow:ioctl(ingests, {add_remote_ingest, StreamAndVariant, Url}, Handle)
  end.

removeVariantImpl(Handle, StreamAndVariant) ->
  fun() ->
      ok = id3as_workflow:ioctl(ingests, {remove_ingest, StreamAndVariant}, Handle)
  end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
startWorkflow(SlotName, StreamNames) ->

  %% TODO - need to close down the PIDS
  Pids = lists:map(fun({StreamAndVariant, _StreamName}) ->
                       {ok, Pid} = webrtc_stream_server:start_link(StreamAndVariant, #{stream_module => rtsv2_webrtc_stream_handler,
                                                                                       stream_module_args => [StreamAndVariant]}),
                       Pid
                   end,
                   StreamNames),

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

  OpusSSRC = 10,
  OpusEncodingId = 111,

  H264SSRC = 20,
  H264EncodingId = 125,

  Workflow = #workflow{
                name = {ingest_aggregator_instance, SlotName},
                display_name = <<"RTMP Ingest Aggregator">>,
                tags = #{type => ingest_aggregator,
                         slot => SlotName},
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

                              [
                               #compound_processor{
                                  name = binary_to_atom(StreamName, utf8),
                                  display_name = StreamName,
                                  spec = #processor_spec{consumes = ?frames},
                                  subscribes_to = {aggregate, ?frames_with_source_id(StreamAndVariant)},
                                  processors = [
                                                #processor{name = audio_decode,
                                                           display_name = <<"Audio Decode">>,
                                                           subscribes_to = {outside_world, ?audio_frames},
                                                           module = fdk_aac_decoder
                                                          },

                                                #processor{name = audio_levels_gain,
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
                                                                                                  bus_name = {audio_levels, StreamAndVariant}}
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
                                                                                             { rtp_opus_egest:new(OpusSSRC, OpusEncodingId)
                                                                                             , rtp_h264_egest:new(H264SSRC, H264EncodingId)
                                                                                             }
                                                                                         , function = fun mux_to_rtp/2
                                                                                         , spec = #processor_spec{ consumes = ?frames,
                                                                                                                   generates = ?rtp_sequences }
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
                                                   name = webrtc_bus_publish,
                                                   display_name = <<"WebRTC Publish">>,
                                                   subscribes_to = [?previous, {outside_world, ?program_details_frames}],
                                                   module = send_to_bus_processor,
                                                   config = #send_to_bus_processor_config{
                                                               consumes = true,
                                                               bus_name = {webrtc_stream_output, StreamAndVariant}
                                                              }
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
                               || {StreamAndVariant, StreamName} <- StreamNames
                              ],

                              %% TODO - remove once complete - shouldn't be getting stray output messages
                              [
                               #processor{
                                  name = binary_to_atom(<<StreamName/binary, "null">>, utf8),
                                  subscribes_to = binary_to_atom(StreamName, utf8),
                                  module = dev_null_processor
                                 }
                               || {_StreamAndVariant, StreamName} <- StreamNames
                              ]
                             ]
               },

  {ok, Pid} = id3as_workflow:start_link(Workflow, self()),

  {ok, Handle} = id3as_workflow:workflow_handle(Pid),

  Handle.

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
