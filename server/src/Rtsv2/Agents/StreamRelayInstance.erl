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
   [ startWorkflowFFI/1
   , applyPlanFFI/2
   , getSlotConfigurationFFI/1
   , setSlotConfigurationFFI/2
   ]).


-define(metadata, rtsv2_agents_streamRelayInstance_metadata).

-record(?metadata,
        { slot_configuration :: rtsv2_slot_configuration:slot_configuration()
        }).


startWorkflowFFI(SlotId) ->
  fun() ->
      start_workflow(SlotId)
  end.


applyPlanFFI(WorkflowHandle, StreamRelayPlan) ->
  fun() ->
      {ok, Result} = id3as_workflow:ioctl(relays, {apply_plan, StreamRelayPlan}, WorkflowHandle),
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

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
start_workflow(SlotId) ->

  %% NOTE: the stream relay generator does all of its forwarding internally, nothing
  %% comes out of it.
  Workflow =
    #workflow{
       name = {stream_relay_instance, SlotId},
       display_name = <<"Stream Relay Workflow for ", (integer_to_binary(SlotId))/binary>>,
       tags = #{ type => stream_relay
               , slot => SlotId
               },
       generators =
         [ #generator{ name = relays
                     , display_name = <<"Relays">>
                     , module = rtsv2_stream_relay_generator
                     }
         ]
      },

  {ok, Pid} = id3as_workflow:start_link(Workflow, self()),

  {ok, Handle} = id3as_workflow:workflow_handle(Pid),

  Handle.
