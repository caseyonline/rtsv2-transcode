-module(rtsv2_stream_relay_merge_redundant_streams_processor).

-include_lib("id3as_media/include/id3as_workflow.hrl").

-behaviour(workflow_processor).

-export([ spec/1
        , initialise/1
        , process_input/2
        , flush/1
        , ioctl/2
        ]).

-define(state, ?MODULE).

-record(?state,
        {
        }).

%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{
     consumes = ?all,
     generates = ?all,
     supports_synchronous_mode = true,
     is_pure = true
    }.

initialise(_Processor) ->
  {ok, #?state{}}.

process_input(Input, State) ->
  {ok, Input, State}.

flush(State) ->
  {flush_complete, State}.

ioctl(_, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
