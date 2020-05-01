-module(rtsv2_ingest_processor).

-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/bitrate_monitor.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").

-include("./rtsv2_ingest_qos_monitor.hrl").

-export([
         ingest_processor/3
        ]).

ingest_processor(IngestKey, ProfileName, Subscriptions) ->

  #{ qosPollIntervalMs := QosPollIntervalMs
   , abortIfNoMediaMs := AbortIfNoMediaMs} = (rtsv2_config@ps:ingestInstanceConfig())(),

  #compound_processor{
     spec = #processor_spec{
               consumes = ?all
              }
    , subscribes_to = Subscriptions
    , processors = [ #processor{ name = set_source_id
                               , subscribes_to = outside_world
                               , module = set_source_id
                               , config = {ProfileName, make_ref()}
                               }

                   , #processor{ name = program_details
                               , subscribes_to = ?previous
                               , module = program_details_generator
                               }

                     %% Updates frame to add estimated bitrate,
                     %% and outputs #bitrate_info messages
                   , #processor{ name = source_bitrate_monitor
                               , display_name = <<"Source Bitrate Monitor">>
                               , subscribes_to = ?previous
                               , module = stream_bitrate_monitor
                               , config = #bitrate_monitor_config{ default_profile_name = source
                                                                 , mode = passthrough_with_update
                                                                 , output_bitrate_info_messages = false
                                                                 }
                               }

                   , #processor{ name = send_to_bus
                               , subscribes_to = {?previous, ?frames}
                               , module = send_to_bus_processor
                               , config = #send_to_bus_processor_config{ consumes = true
                                                                       , bus_name = {ingest, IngestKey}}
                               }

                     %%--------------------------------------------------
                     %% Monitoring
                     %%--------------------------------------------------
                   , #processor{ name = ingest_qos_monitor
                               , display_name = <<"QOS Monitor">>
                               , subscribes_to = set_source_id
                               , module = rtsv2_ingest_qos_monitor
                               , config = #rtsv2_ingest_qos_monitor_config{ poll_interval_ms = QosPollIntervalMs
                                                                          , abort_if_no_media_in_ms = AbortIfNoMediaMs
                                                                          }
                               }

                     %%--------------------------------------------------
                     %% Reporting
                     %%--------------------------------------------------

                     %% Count inbound frames - no output, just meters
                   , #processor{ name = source_frame_meter
                               , display_name = <<"Source Frame Meter">>
                               , subscribes_to = source_bitrate_monitor
                               , module = frame_flow_meter
                               }

                     %% Extracts source details (frame rates etc) for reporting purposes
                     %% Generates #source_info{} records
                   , #processor{ name = source_details_extractor
                               , display_name = <<"Source Details Extractor">>
                               , subscribes_to = source_bitrate_monitor
                               , module = source_details_extractor
                               }
                   ]
    }.
