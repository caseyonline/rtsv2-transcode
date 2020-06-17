-module(rtsv2_ingest_processor).

-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/bitrate_monitor.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").

-include("./rtsv2_ingest_qos_monitor.hrl").

-export([
         ingest_processor/4
        ]).

ingest_processor(IngestKey, ProfileName, SlotProfile, Subscriptions) ->

  IngestInstanceConfig = (rtsv2_config@ps:ingestInstanceConfig())(),

  #compound_processor{
     spec = #processor_spec{
               consumes = ?all
              }
    , subscribes_to = Subscriptions
    , name = ingest_processor
    , processors = [ #processor{ name = set_video_stream_id
                               , subscribes_to = {outside_world, ?video_frames}
                               , module = set_stream_id
                               , config = 256
                               }

                   , #processor{ name = set_audio_stream_id
                               , subscribes_to = {outside_world, ?audio_frames}
                               , module = set_stream_id
                               , config = 257
                               }

                   , #processor{ name = set_script_stream_id
                               , subscribes_to = {outside_world, ?script_frames}
                               , module = set_stream_id
                               , config = 258
                               }

                   , #processor{ name = set_source_id
                               , subscribes_to = [set_video_stream_id, set_audio_stream_id, set_script_stream_id]
                               , module = set_source_id
                               , config = {ProfileName, make_ref()}
                               }

                   , #processor{ name = program_details
                               , subscribes_to = ?previous
                               , module = program_details_generator
                               }

                   , #processor{ name = send_to_bus
                               , subscribes_to = ?previous
                               , module = send_to_bus_processor
                               , config = #send_to_bus_processor_config{ consumes = true
                                                                       , bus_name = {ingest, IngestKey}}
                               }

                     %%--------------------------------------------------
                     %% Monitoring
                     %%--------------------------------------------------

                     %% Outputs #bitrate_info messages
                   , #processor{ name = average_bitrate_monitor
                               , display_name = <<"Average Bitrate Monitor">>
                               , subscribes_to = set_source_id
                               , module = stream_bitrate_monitor
                               , config = #bitrate_monitor_config{ default_profile_name = source
                                                                 , mode = consumes
                                                                 , time_source = streamtime
                                                                 , output_bitrate_info_messages = true
                                                                 , group_bitrate_info_messages = true
                                                                 }
                               }

                     %% Outputs #bitrate_info messages
                   , #processor{ name = peak_bitrate_monitor
                               , display_name = <<"Peak Bitrate Monitor">>
                               , subscribes_to = set_source_id
                               , module = stream_bitrate_monitor
                               , config = #bitrate_monitor_config{ default_profile_name = source
                                                                 , mode = consumes
                                                                 , time_source = streamtime
                                                                 , output_bitrate_info_messages = true
                                                                 , group_bitrate_info_messages = true
                                                                 , window_size = 200
                                                                 , notification_frequency = 200
                                                                 }
                               }

                     %% Monitors #frame and #bitrate_info messages to determine ingest health
                   , #processor{ name = ingest_qos_monitor
                               , display_name = <<"QOS Monitor">>
                               , subscribes_to = [set_source_id, average_bitrate_monitor, peak_bitrate_monitor]
                               , module = rtsv2_ingest_qos_monitor %% - TODO default 5000 kills webrtc sometimes? Perhaps initial poll should be slower?
                               , config = #rtsv2_ingest_qos_monitor_config{ ingestInstanceConfig = IngestInstanceConfig
                                                                          , slotProfile = SlotProfile
                                                                          }
                               }

                     %%--------------------------------------------------
                     %% Reporting
                     %%--------------------------------------------------

                     %% Count inbound frames - no output, just meters
                   , #processor{ name = source_frame_meter
                               , display_name = <<"Source Frame Meter">>
                               , subscribes_to = set_source_id
                               , module = frame_flow_meter
                               }

                     %% Extracts source details (frame rates etc) for reporting purposes
                     %% Generates #source_info{} records
                   , #processor{ name = source_details_extractor
                               , display_name = <<"Source Details Extractor">>
                               , subscribes_to = program_details
                               , module = source_details_extractor
                               }
                   ]
    }.
