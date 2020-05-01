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
                              rtsv2_ingest_processor:ingest_processor(Key, ProfileName, webrtc_ingest)
                             ]
               },

  {ok, WorkflowPid} = id3as_workflow:start_link(Workflow),

  {ok, WorkflowPid}.
