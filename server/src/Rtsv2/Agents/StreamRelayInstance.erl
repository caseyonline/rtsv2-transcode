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

%% Example Plan
%%
%% #{downstreamRelaySinks => [],
%%                               egestSink =>
%%                                   #{destinations =>
%%                                         [#{port => 50319,
%%                                            server => <<"172.16.171.1">>}],
%%                                     source =>
%%                                         {egestSinkSourceIngestAggregator}},
%%                               ingestAggregatorSource =>
%%                                   {ingestAggregatorSourceEnabled,
%%                                       <<"172.16.171.5">>},
%%                               upstreamRelaySources => []}


applyPlanFFI(WorkflowHandle,
             #{ ingestAggregatorSource := IngestAggregatorSource
              , upstreamRelaySources := _UpstreamRelaySourceList
              , egestSink := #{ destinations := EgestDestinationList
                              , source := _EgestSource
                              }
              , downstreamRelaySinks := _DownstreamRelaySinks
              } = StreamRelayPlan) ->
  fun() ->
      io:format(user, "Received a StreamRelayPlan: ~p~n~n", [StreamRelayPlan]),

      IngestAggregatorReceivePort =
        case IngestAggregatorSource of
          {ingestAggregatorSourceEnabled, _IngestAggregatorHost} ->
            {ok,   ReceivePort} = id3as_workflow:ioctl(sources, ensure_ingest_aggregator_source, WorkflowHandle),
            {just, ReceivePort};
          {ingestAggregatorSourceDisabled} ->
            {nothing}
        end,

      lists:foreach(fun(#{ port := EgestPort, server := EgestHost } = _EgestDestination) ->
                        case id3as_workflow:ioctl(egests, {register_egest, EgestHost, EgestPort}, WorkflowHandle) of
                          ok ->
                            ok;
                          {ok, {error, already_registered}} ->
                            ok
                        end
                    end,
                    EgestDestinationList
                   ),

      #{ ingestAggregatorReceivePort => IngestAggregatorReceivePort
       , upstreamRelayReceivePorts => #{}
       }
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

  %% TODO: the actual switchboard logic that dictates what goes where

  Workflow =
    #workflow{
       name = {stream_relay_instance, SlotId},
       display_name = <<"Stream Relay Workflow for ", (integer_to_binary(SlotId))/binary>>,
       tags = #{ type => stream_relay
               , slot => SlotId
               },
       generators =
         [ #generator{ name = sources
                     , display_name = <<"Ingests">>
                     , module = rtsv2_stream_relay_generator
                     }
         ],

       processors =
         [ #processor{ name = egests
                     , display_name = <<"Egests">>
                     , module = rtsv2_relay_to_egest_forward_processor
                     , subscribes_to = sources
                     , config = SlotId
                     }
         %% , #processor{ name = relays
         %%             , display_name = <<"Relays">>
         %%             , module = rtsv2_relay_to_relay_forward_processor
         %%             , subscribes_to = sources
         %%             }
         ]
      },

  {ok, Pid} = id3as_workflow:start_link(Workflow, self()),

  {ok, Handle} = id3as_workflow:workflow_handle(Pid),

  Handle.
