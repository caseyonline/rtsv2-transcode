-module(rtsv2_agents_streamRelayInstance@foreign).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/transcode.hrl").
-include_lib("id3as_media/include/audio_levels.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").
-include_lib("id3as_media/include/fun_processor.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").
-include("../../../../src/rtsv2_slot_media_source_publish_processor.hrl").


-export(
   [ startOriginWorkflowFFI/1
   , applyOriginPlanFFI/2

   , startDownstreamWorkflowFFI/1
   , applyDownstreamPlanFFI/2

   , getSlotConfigurationFFI/1
   , setSlotConfigurationFFI/2

   , stopWorkflowFFI/1
   ]).


-define(metadata, rtsv2_agents_streamRelayInstance_metadata).

-record(?metadata,
        { slot_configuration :: slot_configuration()
        }).


startOriginWorkflowFFI(SlotId) ->
  fun() ->
      start_workflow(origin, SlotId)
  end.

startDownstreamWorkflowFFI(SlotId) ->
  fun() ->
      start_workflow(downstream, SlotId)
  end.

applyOriginPlanFFI(#{ egests := EgestList, downstreamRelays := RelayList } = StreamRelayPlan, WorkflowHandle) ->
  fun() ->
      io:format(user, "ORIGIN STREAM PLAN: ~p~n~n", [StreamRelayPlan]),

      %% Set up all sources and downstream relay fowarding
      {ok, Result} = id3as_workflow:ioctl(source, {apply_origin_plan, StreamRelayPlan}, WorkflowHandle),

      %% Set up all forwarding
      {ok, _FailedResolution} = id3as_workflow:ioctl(forward_to_egests, {set_destinations, EgestList}, WorkflowHandle),
      {ok, _FailedResolution} = id3as_workflow:ioctl(forward_to_relays, {set_destinations, RelayList}, WorkflowHandle),

      Result
  end.

applyDownstreamPlanFFI(#{ egests := EgestList, downstreamRelays := RelayList } = StreamRelayPlan, WorkflowHandle) ->
  fun() ->
      io:format(user, "DOWNSTREAM STREAM PLAN: ~p~n~n", [StreamRelayPlan]),

      %% Set up all sources and downstream relay fowarding
      {ok, Result} = id3as_workflow:ioctl(source, {apply_downstream_plan, StreamRelayPlan}, WorkflowHandle),

      %% Set up all egest forwarding
      {ok, _FailedResolution} = id3as_workflow:ioctl(forward_to_egests, {set_destinations, EgestList}, WorkflowHandle),
      {ok, _FailedResolution} = id3as_workflow:ioctl(forward_to_relays, {set_destinations, RelayList}, WorkflowHandle),

      Result
  end.


setSlotConfigurationFFI(RelayKey, SlotConfiguration) ->
  fun() ->
      _ = gproc:add_local_property({metadata, RelayKey},
                                   #?metadata{ slot_configuration = SlotConfiguration }
                                  )
  end.


getSlotConfigurationFFI(RelayKey) ->
  fun() ->
      case gproc:lookup_local_properties({metadata, RelayKey}) of
        [] ->
          {nothing};

        [{_Pid, #?metadata{ slot_configuration = SlotConfiguration }}] ->
          {just, SlotConfiguration}
      end
  end.

stopWorkflowFFI(WorkflowHandle) ->
  fun() ->
      id3as_workflow:stop(WorkflowHandle)
  end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
start_workflow(OriginOrDownstream, SlotId) ->
  Workflow =
    #workflow{ name = {stream_relay_instance, SlotId}
             , display_name = <<"Stream Relay Workflow for ", (rtsv2_types:uuid_to_string(SlotId))/binary>>
             , tags = #{ type => stream_relay
                       , slot => SlotId
                       }
             , generators =
                 [ #generator{ name = source
                             , display_name = <<"Relays">>
                             , module = rtsv2_stream_relay_generator
                             , config = #{ origin_or_downstream => OriginOrDownstream }
                             }
                 ]
             , processors =
                 [ #processor{ name = merge_redundant_streams_for_egest
                             , display_name = <<"Merge Redundant Streams">>
                             , module = rtsv2_stream_relay_merge_redundant_streams_processor
                             , subscribes_to = source
                             }
                 , #processor{ name = forward_to_egests
                             , display_name = <<"Forward Merged Stream to Egest Instances">>
                             , module = rtsv2_stream_relay_forward_processor
                             , subscribes_to = merge_redundant_streams_for_egest
                             }
                 , #processor{ name = forward_to_relays
                             , display_name = <<"Forward Merged Stream to Downstream Relays">>
                             , module = rtsv2_stream_relay_forward_processor
                             , subscribes_to = merge_redundant_streams_for_egest
                             }
                 ]
             },

  {ok, Pid} = id3as_workflow:start_link(Workflow, self()),

  {ok, Handle} = id3as_workflow:workflow_handle(Pid),

  Handle.
