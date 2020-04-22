-module(rtsv2_agents_ingestWebRTCIngestHandler@foreign).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/rtmp.hrl").
-include_lib("id3as_media/include/bitrate_monitor.hrl").
-include_lib("id3as_media/include/receive_from_bus_generator.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").
-include_lib("id3as_avp/include/avp_ingest_source_details_extractor.hrl").

-export([ startWorkflowImpl/1
        ]).

startWorkflowImpl(IngestKey) ->
  fun() ->
      {ok, WorkflowPid} = start_workflow(IngestKey),
      WorkflowPid
  end.

%%------------------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------------------
start_workflow(Key = {ingestKey, SlotId, SlotRole, ProfileName}) ->

  BusName = {rtsv2_webrtc_ingest_bus, SlotId, SlotRole, ProfileName},

  Workflow = #workflow{
                name = {webrtc_ingest_handler, Key},
                display_name = <<"WebRTC Ingest">>,
                tags = #{type => webrtc_ingest_handler,
                         slot => SlotId,
                         stream_role => SlotRole,
                         profile => ProfileName
                        },
                generators = [
                              #generator{name = webrtc_ingest,
                                         module = receive_from_bus_generator,
                                         config = #receive_from_bus_generator_config{bus_name = BusName}
                                        }
                             ],
                processors = [
                              #processor{name = set_source_id,
                                         subscribes_to = webrtc_ingest,
                                         module = set_source_id,
                                         config = {ProfileName, make_ref()}
                                        },

                              %% #processor{name = reorder_slices,
                              %%            subscribes_to = ?previous,
                              %%            module = h264_reorder_slices
                              %%           },

                              #processor{name = program_details,
                                         subscribes_to = ?previous,
                                         module = program_details_generator
                                        },

                              %% Updates frame to add estimated bitrate,
                              %% and outputs #bitrate_info messages
                              #processor{name = source_bitrate_monitor,
                                         display_name = <<"Source Bitrate Monitor">>,
                                         subscribes_to = ?previous,
                                         module = stream_bitrate_monitor,
                                         config = #bitrate_monitor_config{
                                                     default_profile_name = source,
                                                     mode = passthrough_with_update,
                                                     output_bitrate_info_messages = false
                                                    }
                                        },

                              #processor{name = send_to_bus,
                                         subscribes_to = {?previous, ?frames},
                                         module = send_to_bus_processor,
                                         config = #send_to_bus_processor_config{consumes = true,
                                                                                bus_name = {ingest, Key}}
                                        },

                              %%--------------------------------------------------
                              %% Reporting
                              %%--------------------------------------------------

                              %% Count inbound frames - no output, just meters
                              #processor{name = source_frame_meter,
                                         display_name = <<"Source Frame Meter">>,
                                         subscribes_to = source_bitrate_monitor,
                                         module = frame_flow_meter},

                              %% Extracts source details (frame rates etc) for reporting purposes
                              %% Generates #source_info{} records
                              #processor{name = source_details_extractor,
                                         display_name = <<"Source Details Extractor">>,
                                         subscribes_to = source_bitrate_monitor,
                                         module = source_details_extractor},

                              %% #processor{name = writer,
                              %%            subscribes_to = {reorder_slices, ?video_frames},
                              %%            module = frame_writer,
                              %%            config = #frame_writer_config{filename = "/tmp/out.h264", mode = consumes}
                              %%           }

                              []
                             ]
               },

  {ok, WorkflowPid} = id3as_workflow:start_link(Workflow),

  {ok, WorkflowPid}.
