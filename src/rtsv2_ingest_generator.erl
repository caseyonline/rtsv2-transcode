-module(rtsv2_ingest_generator).

-behaviour(workflow_generator).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/llwp.hrl").
-include_lib("id3as_media/include/receive_from_bus_generator.hrl").

-include("./rtsv2_types.hrl").

-export([
         init/1,
         handle_info/2,
         ioctl/2
        ]).

-define(state, ?MODULE).

-record(?state, { workflow_context :: workflow_context(),
                  workflows :: maps:map(ingest_key(), id3as_workflow:workflow_handle())
                }).

%%------------------------------------------------------------------------------
%% generator callbacks
%%------------------------------------------------------------------------------
init(#generator{workflow_context = Context}) ->

  {ok, #?state{workflow_context = Context,
               workflows = #{}}}.

%% handle_info(X = #workflow_output{message = flush_complete}, State) ->
%%   io:format(user, "FLUSH COMPLETE
%%   {noreply, State};

handle_info(#workflow_output{message = Msg}, State) ->
  {output, Msg, State};

handle_info(Msg = {ingest_stopped, _}, State) ->
  {output, Msg, State}.

ioctl({add_local_ingest, IngestKey}, State = #?state{workflows = Workflows,
                                                            workflow_context = Context}) ->

  NewHandle = start_workflow_for_local_ingest(IngestKey, Context),

  {ok, State#?state{workflows = maps:put(IngestKey, NewHandle, Workflows)}};

ioctl({add_remote_ingest, IngestKey, Url}, State = #?state{workflows = Workflows,
                                                                  workflow_context = Context}) ->

  NewHandle = start_workflow_for_remote_ingest(Url, Context),

  {ok, State#?state{workflows = maps:put(IngestKey, NewHandle, Workflows)}};

ioctl({remove_ingest, IngestKey}, State = #?state{workflows = Workflows}) ->

  Handle = maps:get(IngestKey, Workflows),

  ok = id3as_workflow:stop(Handle),

  self() ! {ingest_stopped, IngestKey},

  {ok, State#?state{workflows = maps:remove(IngestKey, Workflows)}}.

start_workflow_for_local_ingest(IngestKey, Context) ->
  Workflow = #workflow{
                name = ingest_instance,
                display_name = <<>>,
                generators = [
                              #generator{name = bus,
                                         display_name = <<"Receive from Bus">>,
                                         module = receive_from_bus_generator,
                                         config = #receive_from_bus_generator_config{ bus_name = {ingest, IngestKey}
                                                                                    , send_program_details_from_metadata_on_startup = true
                                                                                    }
                                        }
                             ],
                processors = [
                             ]
               },

  {ok, Pid} = id3as_workflow:start_link(Workflow, self(), Context),

  {ok, Handle} = id3as_workflow:workflow_handle(Pid),

  Handle.

start_workflow_for_remote_ingest(Url, Context) ->
  Workflow = #workflow{
                name = ingest_instance,
                display_name = <<>>,
                generators = [
                              #generator{name = llwp,
                                         module = llwp_generator,
                                         config = #llwp_generator_config{url = Url}
                                        }
                             ],
                processors = [
                             ]
               },

  {ok, Pid} = id3as_workflow:start_link(Workflow, self(), Context),

  {ok, Handle} = id3as_workflow:workflow_handle(Pid),

  Handle.
