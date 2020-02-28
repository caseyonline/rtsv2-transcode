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
-type source_key() :: list(pop_name()).

-record(?state,
        { workflow_context :: workflow_context()
        , workflows :: maps:map(source_key(), id3as_workflow:workflow_handle())
        }).

%%------------------------------------------------------------------------------
%% Generator API
%%------------------------------------------------------------------------------
init(#generator{workflow_context = Context}) ->
  { ok
  , #?state{ workflow_context = Context
           , workflows = #{}
           }
  }.

handle_info(#workflow_output{message = Msg}, State) ->
  {output, Msg, State}.


ioctl(ensure_ingest_aggregator_source, State = #?state{ workflows = Workflows, workflow_context = Context }) ->

  %% There can only ever be one ingest aggregator for a stream, so that's sufficiently
  %% unique to use as a key
  SourceKey = ingest_aggregator,

  {Handle, FinalState} =
    case maps:find(SourceKey, Workflows) of
      {ok, ExistingHandle} ->
        {ExistingHandle, State};

      error ->
        {ok, NewHandle} = start_workflow_for_ingest_aggregator_source(Context),
        NewState = State#?state{workflows = maps:put(SourceKey, NewHandle, Workflows)},
        {NewHandle, NewState}
    end,

  {ok, Port} = id3as_workflow:ioctl(source, get_port_number, Handle),
  {ok, Port, FinalState};

ioctl({ensure_upstream_relay_source, UpstreamRelay}, State = #?state{ workflows = Workflows, workflow_context = Context }) ->

  SourceKey = {stream_relay, UpstreamRelay},

  {Handle, FinalState} =
    case maps:find(SourceKey, Workflows) of
      {ok, ExistingHandle} ->
        {ExistingHandle, State};

      error ->
        {ok, NewHandle} = start_workflow_for_stream_relay_source(UpstreamRelay, Context),
        NewState = State#?state{workflows = maps:put(SourceKey, NewHandle, Workflows)},
        {NewHandle, NewState}
    end,

  {ok, Port} = id3as_workflow:ioctl(source, get_port_number, Handle),
  {ok, Port, FinalState}.


%%------------------------------------------------------------------------------
%% Private Functions
%%------------------------------------------------------------------------------
start_workflow_for_ingest_aggregator_source(Context) ->
  Workflow =
    #workflow{ name = ingest_aggregator_source
             , display_name = <<>>
             , generators =
                 [ #generator{ name = source
                             , display_name = <<"Receive from Ingest Aggregator">>
                             , module = rtsv2_rtp_trunk_receiver_generator
                             }
                 ]
             },

  {ok, Pid} = id3as_workflow:start_link(Workflow, self(), Context),

  {ok, Handle} = id3as_workflow:workflow_handle(Pid),

  {ok, Handle}.


start_workflow_for_stream_relay_source(_UpstreamRelay, Context) ->
  Workflow =
    #workflow{ name = upstream_relay_source
             , display_name = <<>>
             , generators =
                 [ #generator{ name = source
                             , display_name = <<"Receive from Stream Relay">>
                             , module = rtsv2_rtp_trunk_receiver_generator
                             }
                 ]
             },

  {ok, Pid} = id3as_workflow:start_link(Workflow, self(), Context),

  {ok, Handle} = id3as_workflow:workflow_handle(Pid),

  {ok, Handle}.
