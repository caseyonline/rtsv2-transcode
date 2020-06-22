-module(rtsv2_rtmp_profiles_bus_processor).

-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/transcode.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").
-include_lib("id3as_media/include/stream_sync.hrl").
-include("./rtsv2_rtp.hrl").
-include("./rtsv2_bus_messages.hrl").

-export([
  spec/1,
  initialise/1,
  process_input/2,
  handle_info/2,
  ioctl/2
]).

-behaviour(workflow_processor).

-define(state, ?MODULE).

-record(?state, {
  workflow :: undefined | pid()
}).

%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{
     consumes = ?frames,
     generates = ?all
    }.

initialise(_Processor = #processor{ config = Config }) ->
  {ok, #?state{
  }}.

process_input(_Input, State = #?state{ workflow = undefined }) ->
  {ok, State};
process_input(Input, State = #?state{ workflow = Workflow }) ->
  ok = id3as_workflow:process_input(Input, Workflow),
  {ok, State}.

ioctl({slot_configuration, SlotConfiguration}, State = #?state{ workflow = undefined }) ->
  {ok, WorkflowPid} = start_workflow(SlotConfiguration),
  {ok, State#?state{ workflow = WorkflowPid}}.


handle_info(#workflow_output{message = Msg}, State) ->
  ?INFO("rtmp thingy other output msg ~p", [Msg]),
  {output, Msg, State}.

start_workflow(SlotConfiguration = #{ profiles := Profiles, slotId := SlotId, slotRole := SlotRole }) ->
  ?INFO("Starting workflow for slot configuration ~p", [SlotConfiguration]),
  EgestKey = {egestKey, SlotId, SlotRole}, %% TODO
  AudioConfig = #audio_transcode_config{
                  input = #frame_spec{
                            profile = #audio_profile{}
                            },
                  outputs = [
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
                            ]
                },
  Workflow = #workflow{
                name = rtmp_bus,
                processors = [
                              [
                                processor(outside_world, AudioConfig, EgestKey, Profile) 
                              || Profile <- Profiles
                              ]
                ]
  },
  id3as_workflow:start_link(Workflow).

processor(SubscribeNode, AudioConfig, EgestKey, #{firstAudioSSRC := AudioSSRC, firstVideoSSRC := VideoSSRC, profileName := ProfileName}) ->
  #compound_processor{
    name = binary_to_atom(ProfileName, utf8),
    spec = #processor_spec{consumes = [?frames]},
    subscribes_to = [{SubscribeNode, ?audio_frames_with_stream_id(AudioSSRC)},  
                     {SubscribeNode, ?video_frames_with_stream_id(VideoSSRC)}
                    ],
    processors = [
      #processor{name = transcode_audio,
                subscribes_to = {outside_world, ?audio_frames},
                module = audio_transcode,
                config = AudioConfig
          },
      #processor{name = set_audio_stream_id,
                  subscribes_to = ?previous,
                  module = set_stream_id,
                  config = 2
              },
      
      #processor{name = set_video_stream_id,
                subscribes_to = {outside_world, ?video_frames},
                module = set_stream_id,
                config = 1
                },

      #processor{name = stream_sync,
                  subscribes_to = [set_audio_stream_id, set_video_stream_id],
                  module = stream_sync,
                  config = #stream_sync_config{ num_streams = 2,
                              stream_key = fun (#frame{type = Type, stream_metadata = #stream_metadata{stream_id = StreamId}}) -> {Type, StreamId} end 
                          }
                },
      
      #processor{name = flv,
                  display_name = <<"FLV Frame Generator">>,
                  subscribes_to = ?previous,
                  module = flv_frame_generator
                },

      #processor{name = send_to_bus,
                display_name = <<"Send to RTMP Bus">>,
                subscribes_to = ?previous,
                module = send_to_bus_processor,
                config = #send_to_bus_processor_config{consumes = true,
                                                        bus_name = ?RTMP_EGEST_BUS(EgestKey, ProfileName)}
                },
      #processor{name = input_frame_meter,
        display_name = <<"Input Frame Meter">>,
        subscribes_to = outside_world,
        module = frame_flow_meter},
      #processor{name = output_frame_meter,
        display_name = <<"Output Frame Meter">>,
        subscribes_to = stream_sync,
        module = frame_flow_meter}


    ]
  }.
