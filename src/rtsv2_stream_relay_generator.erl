-module(rtsv2_stream_relay_generator).

-behaviour(workflow_generator).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").

-export([ init/1
        , handle_info/2
        , ioctl/2
        ]).

-define(origin_state, rtsv2_stream_relay_generator_origin_state).
-define(downstream_state, rtsv2_stream_relay_generator_downstream_state).

-type pop_name() :: binary_string().
-type relay_key() :: #{ next := pop_name(), rest := list(pop_name()) }.

-record(?origin_state,
        { workflow_context :: workflow_context()
        , ingest_aggregator_workflow :: undefined | id3as_workflow:workflow_handle()
        }).

-record(?downstream_state,
        { workflow_context :: workflow_context()
        , upstream_relay_workflows = #{} :: maps:map(relay_key(), id3as_workflow:workflow_handle())
        }).

%%------------------------------------------------------------------------------
%% Generator API
%%------------------------------------------------------------------------------
init(#generator{workflow_context = Context, config = #{ origin_or_downstream := origin }}) ->
  { ok
  , #?origin_state{ workflow_context = Context }
  };

init(#generator{workflow_context = Context, config = #{ origin_or_downstream := downstream }}) ->
  { ok
  , #?downstream_state{ workflow_context = Context }
  }.


handle_info(#workflow_output{message = Msg}, State) ->
  {output, Msg, State}.


%% Look in StreamRelayInstance.purs for StreamRelayPlan for the structure of a plan
ioctl({ apply_plan
      , { originStreamRelayPlan
        , #{ downstreamRelays := DownstreamRelays }
        }
      },
      #?origin_state{} = State
     ) ->

  {IngestAggregatorResult, NewState} =
    begin
        {ok,    IngestReceivePort, StateWithAggregatorSource} = ensure_ingest_aggregator_source(DownstreamRelays, State),
        {{just, IngestReceivePort}, StateWithAggregatorSource}
    end,

  Result =
    #{ ingestAggregatorReceivePort => IngestAggregatorResult
     , upstreamRelayReceivePorts => #{}
     },

  {ok, Result, NewState};

ioctl({ apply_plan
      , { downstreamStreamRelayPlan
        , #{ upstreamRelaySources := UpstreamRelaySourceList }
        }
      },
      State
     ) ->

  %% Ensure current upstream relays exist and are properly configured
  {NewState1, UpstreamRelayReceivePorts} =
    lists:foldl(fun(#{ relay := UpstreamRelay } = UpstreamRelaySource, {StateIn, RelayReceivePortsIn}) ->
                    {ok, RelayReceivePort, StateOut} = ensure_upstream_relay_source(UpstreamRelaySource, StateIn),
                    {StateOut, [{UpstreamRelay, RelayReceivePort} | RelayReceivePortsIn]}
                end,
                {State, []},
                UpstreamRelaySourceList
               ),

  %% Remove any relays that are no longer relevant
  ValidUpstreamRelays = maps:from_list([ {RelayKey, true} || #{ relay := RelayKey } <- UpstreamRelaySourceList ]),

  NewUpstreamRelayWorkflows =
    maps:fold(fun(ExistingRelayKey, ExistingRelayWorkflowHandle, UpstreamRelayWorkflowsIn) ->
                  case maps:is_key(ExistingRelayKey, ValidUpstreamRelays) of
                    true ->
                      UpstreamRelayWorkflowsIn;
                    false ->
                      _ = id3as_workflow:stop(ExistingRelayWorkflowHandle),
                      maps:remove(ExistingRelayKey, UpstreamRelayWorkflowsIn)
                  end
              end,
              NewState1#?downstream_state.upstream_relay_workflows,
              NewState1#?downstream_state.upstream_relay_workflows
             ),

  NewState2 = NewState1#?downstream_state{ upstream_relay_workflows = NewUpstreamRelayWorkflows },

  Result =
    #{ ingestAggregatorReceivePort => {nothing}
     , upstreamRelayReceivePorts => maps:from_list(UpstreamRelayReceivePorts)
     },

  {ok, Result, NewState2}.

%%------------------------------------------------------------------------------
%% Private Functions
%,%------------------------------------------------------------------------------
ensure_ingest_aggregator_source(DownstreamRelayList, #?origin_state{ ingest_aggregator_workflow = WorkflowHandle } = State) when WorkflowHandle =/= undefined ->
  {ok, ReceivePort} = id3as_workflow:ioctl(source, get_port_number, WorkflowHandle),
  {ok, _RelaysWhichFailedResolution} = id3as_workflow:ioctl(forward_to_relays, {set_destinations, DownstreamRelayList}, WorkflowHandle),
  {ok, ReceivePort, State};

ensure_ingest_aggregator_source(DownstreamRelayList, #?origin_state{ workflow_context = WorkflowContext } = State) ->
  {ok, WorkflowHandle} = start_workflow_for_ingest_aggregator_source(WorkflowContext),
  NewState = State#?origin_state{ ingest_aggregator_workflow = WorkflowHandle },
  ensure_ingest_aggregator_source(DownstreamRelayList, NewState).


start_workflow_for_ingest_aggregator_source(Context) ->
  Workflow =
    #workflow{ name = ingest_aggregator_source
             , display_name = <<"Ingest Aggregator Source">>
             , generators =
                 [ #generator{ name = source
                             , display_name = <<"Receive from Ingest Aggregator">>
                             , module = rtsv2_rtp_trunk_receiver_generator
                             }
                 ]
             , processors =
                 [ #processor{ name = forward_to_relays
                             , display_name = <<"Forward to Downstream Relays">>
                             , module = rtsv2_stream_relay_forward_processor
                             , subscribes_to = source
                             }
                 , #processor{ name = tap_for_egests
                             , display_name = <<"Tap for Egests">>
                             , module = passthrough_processor
                             , subscribes_to = source
                             }
                 ]
             },
  {ok, Pid} = id3as_workflow:start_link(Workflow, self(), Context),
  {ok, Handle} = id3as_workflow:workflow_handle(Pid),
  {ok, Handle}.


ensure_upstream_relay_source(#{ relay := RelayKey
                              , downstreamRelays := DownstreamRelayList
                              } = _UpstreamRelaySource,
                             #?downstream_state{ upstream_relay_workflows = Workflows
                                               , workflow_context = Context
                                               } = State
                            ) ->

  {WorkflowHandle, FinalState} =
    case maps:find(RelayKey, Workflows) of
      {ok, ExistingHandle} ->
        {ExistingHandle, State};

      error ->
        {ok, NewHandle} = start_workflow_for_stream_relay_source(RelayKey, Context),
        NewState = State#?downstream_state{ upstream_relay_workflows = maps:put(RelayKey, NewHandle, Workflows) },
        {NewHandle, NewState}
    end,

  {ok, Port} = id3as_workflow:ioctl(source, get_port_number, WorkflowHandle),
  {ok, _RelaysWhichFailedResolution} = id3as_workflow:ioctl(forward_to_relays, {set_destinations, DownstreamRelayList}, WorkflowHandle),
  {ok, Port, FinalState}.


start_workflow_for_stream_relay_source(RelayKey, Context) ->
  Workflow =
    #workflow{ name = {upstream_relay_source, RelayKey}
             , display_name = <<"Upstream Relay Source">>
             , generators =
                 [ #generator{ name = source
                             , display_name = <<"Receive from Upstream Relay">>
                             , module = rtsv2_rtp_trunk_receiver_generator
                             }
                 ]
             , processors =
                 [ #processor{ name = forward_to_relays
                             , display_name = <<"Forward to Downstream Relays">>
                             , module = rtsv2_stream_relay_forward_processor
                             , subscribes_to = source
                             }
                 , #processor{ name = tap_for_egests
                             , display_name = <<"Tap for Egests">>
                             , module = passthrough_processor
                             , subscribes_to = source
                             }
                 ]
             },
  {ok, Pid} = id3as_workflow:start_link(Workflow, self(), Context),
  {ok, Handle} = id3as_workflow:workflow_handle(Pid),
  {ok, Handle}.
