-module(rtsv2_stream_relay_generator).

-behaviour(workflow_generator).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").

-export([ init/1
        , handle_info/2
        , ioctl/2
        ]).

-define(state, ?MODULE).

-type pop_name() :: binary_string().
-type relay_key() :: #{ next := pop_name(), rest := list(pop_name()) }.

-record(?state,
        { workflow_context :: workflow_context()
        , upstream_relay_workflows :: maps:map(relay_key(), id3as_workflow:workflow_handle())
        , ingest_aggregator_workflow :: undefined | id3as_workflow:workflow_handle()
        }).

%%------------------------------------------------------------------------------
%% Generator API
%%------------------------------------------------------------------------------
init(#generator{workflow_context = Context}) ->
  { ok
  , #?state{ workflow_context = Context
           , upstream_relay_workflows = #{}
           }
  }.

handle_info(#workflow_output{message = Msg}, State) ->
  {output, Msg, State}.


%% Look in StreamRelayInstance.purs for StreamRelayPlan for the structure of a plan
ioctl({ apply_plan
      , #{ ingestAggregatorSource := MaybeIngestAggregatorSource
         , upstreamRelaySources := UpstreamRelaySourceList
         } = _StreamRelayPlan
      },
      State
     ) ->

  io:format(user, "STREAM PLAN: ~p~n~n", [_StreamRelayPlan]),

  {IngestAggregatorResult, NewState1} =
    case MaybeIngestAggregatorSource of
      {just, IngestAggregatorSource} ->
        {ok,    IngestReceivePort, StateWithAggregatorSource} = ensure_ingest_aggregator_source(IngestAggregatorSource, State),
        {{just, IngestReceivePort}, StateWithAggregatorSource};

      {nothing} ->
        {{nothing}, State}
    end,

  %% Ensure current upstream relays exist and are properly configured
  {NewState2, UpstreamRelayReceivePorts} =
    lists:foldl(fun(#{ relay := UpstreamRelay } = UpstreamRelaySource, {StateIn, RelayReceivePortsIn}) ->
                    {ok, RelayReceivePort, StateOut} = ensure_upstream_relay_source(UpstreamRelaySource, StateIn),
                    {StateOut, [{UpstreamRelay, RelayReceivePort} | RelayReceivePortsIn]}
                end,
                {NewState1, []},
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
                      io:format(user, "Removing obsolete stream relay ~p~n", [ExistingRelayKey]),
                      _ = id3as_workflow:stop(ExistingRelayWorkflowHandle),
                      maps:remove(ExistingRelayKey, UpstreamRelayWorkflowsIn)
                  end
              end,
              NewState2#?state.upstream_relay_workflows,
              NewState2#?state.upstream_relay_workflows
             ),

  NewState3 = NewState2#?state{ upstream_relay_workflows = NewUpstreamRelayWorkflows },

  Result =
    #{ ingestAggregatorReceivePort => IngestAggregatorResult
     , upstreamRelayReceivePorts => maps:from_list(UpstreamRelayReceivePorts)
     },

  {ok, Result, NewState3}.


%%------------------------------------------------------------------------------
%% Private Functions
%,%------------------------------------------------------------------------------
ensure_ingest_aggregator_source(#{ egests := EgestList, downstreamRelays := DownstreamRelayList } = _IngestAggregatorSource, #?state{ ingest_aggregator_workflow = WorkflowHandle } = State) when WorkflowHandle =/= undefined ->
  {ok, ReceivePort} = id3as_workflow:ioctl(source, get_port_number, WorkflowHandle),
  {ok, _EgestsWhichFailedResolution} = id3as_workflow:ioctl(forward_to_egests, {set_destinations, EgestList}, WorkflowHandle),
  {ok, _RelaysWhichFailedResolution} = id3as_workflow:ioctl(forward_to_relays, {set_destinations, DownstreamRelayList}, WorkflowHandle),
  {ok, ReceivePort, State};

ensure_ingest_aggregator_source(IngestAggregatorSource, #?state{ workflow_context = WorkflowContext } = State) ->
  {ok, WorkflowHandle} = start_workflow_for_ingest_aggregator_source(WorkflowContext),
  NewState = State#?state{ ingest_aggregator_workflow = WorkflowHandle },
  ensure_ingest_aggregator_source(IngestAggregatorSource, NewState).


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
                 [ #processor{ name = forward_to_egests
                             , display_name = <<"Forward to Egests">>
                             , module = rtsv2_stream_relay_forward_processor
                             , subscribes_to = source
                             }
                 , #processor{ name = forward_to_relays
                             , display_name = <<"Forward to Downstream Relays">>
                             , module = rtsv2_stream_relay_forward_processor
                             , subscribes_to = source
                             }
                 ]
             },
  {ok, Pid} = id3as_workflow:start_link(Workflow, self(), Context),
  {ok, Handle} = id3as_workflow:workflow_handle(Pid),
  {ok, Handle}.


ensure_upstream_relay_source(#{ relay := RelayKey
                              , egests := EgestList
                              , downstreamRelays := DownstreamRelayList
                              } = _UpstreamRelaySource,
                             #?state{ upstream_relay_workflows = Workflows
                                    , workflow_context = Context
                                    } = State
                            ) ->

  {WorkflowHandle, FinalState} =
    case maps:find(RelayKey, Workflows) of
      {ok, ExistingHandle} ->
        {ExistingHandle, State};

      error ->
        {ok, NewHandle} = start_workflow_for_stream_relay_source(RelayKey, Context),
        NewState = State#?state{ upstream_relay_workflows = maps:put(RelayKey, NewHandle, Workflows) },
        {NewHandle, NewState}
    end,

  {ok, Port} = id3as_workflow:ioctl(source, get_port_number, WorkflowHandle),
  {ok, _EgestsWhichFailedResolution} = id3as_workflow:ioctl(forward_to_egests, {set_destinations, EgestList}, WorkflowHandle),
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
                 [ #processor{ name = forward_to_egests
                             , display_name = <<"Forward to Egests">>
                             , module = rtsv2_stream_relay_forward_processor
                             , subscribes_to = source
                             }
                 , #processor{ name = forward_to_relays
                             , display_name = <<"Forward to Downstream Relays">>
                             , module = rtsv2_stream_relay_forward_processor
                             , subscribes_to = source
                             }
                 ]
             },
  {ok, Pid} = id3as_workflow:start_link(Workflow, self(), Context),
  {ok, Handle} = id3as_workflow:workflow_handle(Pid),
  {ok, Handle}.
