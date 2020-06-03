-module(rtsv2_hls_segment_workflow).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/m3u8.hrl").
-include_lib("id3as_media/include/ts.hrl").
-include_lib("id3as_media/include/ts_encoder.hrl").
-include_lib("id3as_media/include/ts_segment_generator.hrl").
-include_lib("id3as_media/include/stream_sync.hrl").


-include("rtsv2_slot_media_source_publish_processor.hrl").
-include("rtsv2_rtp.hrl").
-include("rtsv2_types.hrl").
-include("rtsv2_internal_hls_writer.hrl").
-include("gop_measurer.hrl").
-include("rtsv2_hls_segment_workflow.hrl").


-behaviour(workflow_processor).

-export([
         spec/1,
         initialise/1,
         process_input/2,
         flush/1,
         ioctl/2,

         handle_info/2,
         contained_workflow/1
        ]).

-define(INITIAL_GOP_COUNT, 3).
-define(state, ?MODULE).

-define(AUDIO_GOP_LENGTH, 90000).

-record(?state, {
  workflow :: undefined | i3das_workflow:compiled_workflow(),
  config :: rtsv2_hls_segment_workflow_config(),
  gop_duration :: integer(),
  initial_gop_count = 0 :: integer()
}).


%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{
     consumes = ?all,
     generates = ?all
    }.


initialise(_Processor = #processor{config = Config}) ->
  {ok, #?state{ config = Config }}.


process_input(Input, State = #?state{ workflow = undefined, config = Config = #rtsv2_hls_segment_workflow_config{ audio_only = true }}) ->
  ?INFO("Building audio-only workflow"),
  Workflow = build_workflow(?AUDIO_GOP_LENGTH, Config),

  {ok, Pid} = id3as_workflow:start_link(Workflow),
  {ok, WorkflowState} = id3as_workflow:workflow_state(Pid),
  State2 = State#?state{ workflow = WorkflowState, gop_duration = ?AUDIO_GOP_LENGTH },
  process_input(Input, State2);

process_input(#gop_measurement{ duration = Duration }, State = #?state{ workflow = undefined, gop_duration = ExistingDuration, initial_gop_count = GopCount }) when GopCount+1 < ?INITIAL_GOP_COUNT ->
  NewDuration = max(Duration, ?null_coalesce(ExistingDuration,0)),
  ?INFO("Calculated GOP duration as ~p from ~p, ~p (~p)", [NewDuration, ExistingDuration, Duration, GopCount]),
  {ok, State#?state{ gop_duration = NewDuration, initial_gop_count = GopCount+1 }};

process_input(#gop_measurement{ duration = Duration }, State = #?state{ workflow = undefined, config = Config, gop_duration = ExistingDuration, initial_gop_count = GopCount }) ->
  NewDuration = max(Duration, ?null_coalesce(ExistingDuration,0)),
  ?INFO("Calculated GOP duration as ~p from ~p, ~p (~p) - now building workflow", [NewDuration, ExistingDuration, Duration, GopCount]),
  Workflow = build_workflow(NewDuration, Config),

  {ok, Pid} = id3as_workflow:start_link(Workflow),
  {ok, WorkflowState} = id3as_workflow:workflow_state(Pid),
  {ok, State#?state{ workflow = WorkflowState, gop_duration = NewDuration, initial_gop_count = GopCount+1 }};

process_input(_Other, State = #?state{ workflow = undefined }) ->
  {ok, State};

process_input(#gop_measurement{}, State = #?state{ workflow = _Workflow }) ->
  {ok, State};

process_input(Frame, State = #?state{ workflow = Workflow }) ->
  ok = id3as_workflow:process_input(Frame, Workflow),
  {ok, State}.

ioctl(undefined, State) ->
  {ok, State}.

handle_info(#workflow_output{message = flush_complete}, State) ->
  {flush_complete, State};

handle_info(#workflow_output{message = Message}, State) ->
  {output, Message, State};

handle_info({Ref, _Msg}, State) when is_reference(Ref) ->
  %% Catch for replies to timed-out gen_server calls
  {noreply, State}.

flush(State = #?state{workflow = Handle}) ->

  ok = id3as_workflow:flush(Handle),

  {flush_in_progress, State}.


contained_workflow(#?state{workflow = undefined}) ->
  false;
contained_workflow(#?state{workflow = Workflow}) ->
  {ok, Workflow}.

build_workflow(GopDurationPts,
               #rtsv2_hls_segment_workflow_config{ slot_profile = #{ profileName := ProfileName },
                                                   push_details = PushDetails = [#{ auth := #{username := Username, password := Password}, segmentDuration := DesiredSegmentDuration, playlistDuration := PlaylistDuration, putBaseUrl := PutBaseUrl } | _ ],
                                                   audio_only = AudioOnly

 } ) ->
  ?INFO("PushDetails: ~p, GopDurationPts: ~p", [PushDetails, GopDurationPts]),
  if length(PushDetails) > 1 -> ?WARNING("Only currently supporting 1 push details: ~p", [PushDetails]);
     ?otherwise -> ok
  end,
  GopDurationS = GopDurationPts / 90000,
  GopsPerSegment = round(DesiredSegmentDuration / GopDurationS),
  SegmentDurationActual = round(GopsPerSegment * GopDurationS),
  
  ?INFO("Requested segment duration of ~p, actual shall be ~p - ~p gops per segment, gop duration ~p", [DesiredSegmentDuration, SegmentDurationActual, GopsPerSegment, GopDurationS]),
  TsEncoderConfig = #ts_encoder_config{
                       which_encoder = ts_simple_encoder,
                       program_pid = 4096,
                       pcr_pid = case AudioOnly of true -> 257; false -> 256 end,
                       streams = 
                        case AudioOnly of 
                          true -> [];
                          false -> [
                            #ts_stream {
                                pid = 256,
                                index = 1,
                                pes_stream_type = ?STREAM_TYPE_VIDEO_H264,
                                pes_stream_id = ?PesVideoStreamId(1)
                              }
                          ]
                         end ++ [ #ts_stream {
                                    pid = 257,
                                    index = 2,
                                    pes_stream_type = ?STREAM_TYPE_AUDIO_AAC_ADTS,
                                    pes_stream_id = ?PesAudioStreamId(1)
                                  }
                                ]
                        },
  #workflow{
            name = rtsv2_hls_segmenter,
            display_name = <<"HLS Segment Publisher">>,
            processors =
              [
                #processor{name = adts,
                  module = adts_encapsulator,
                  subscribes_to = { outside_world, ?audio_frames }
                },
                #processor{
                  name = stream_sync,
                  module = stream_sync,
                  subscribes_to = [{outside_world, ?video_frames}, ?previous],
                  config = #stream_sync_config{ num_streams = case AudioOnly of true -> 1; false -> 2 end }
                },
                #processor{name = ts_mux,
                  display_name = <<"TS Mux">>,
                  subscribes_to = ?previous,
                  module = ts_encoder,
                  config = TsEncoderConfig
                },

                #processor{name = ts_segmenter,
                  display_name = <<"TS Segmenter">>,
                  subscribes_to = ?previous,
                  config = #ts_segment_generator_config {
                              mode = gop_number,
                              segment_strategy = #ts_segment_generator_gop_strategy {
                                gops_per_segment = GopsPerSegment 
                              }
                            },
                  module = ts_segment_generator},

                #processor{name = ts_writer_primary,
                  display_name = <<"TS Segment Writer">>,
                  subscribes_to = ?previous,
                  module = rtsv2_internal_hls_writer,
                  config =
                      #rtsv2_internal_hls_writer_config{
                        post_url = <<PutBaseUrl/binary, ProfileName/binary, "/">>,
                        max_playlist_length = PlaylistDuration div SegmentDurationActual,
                        target_segment_duration = SegmentDurationActual,
                        playlist_name = <<"playlist.m3u8">>,
                        auth = {Username,Password}
                      }
                }
              ]}.
