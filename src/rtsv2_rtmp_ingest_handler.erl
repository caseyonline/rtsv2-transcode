-module(rtsv2_rtmp_ingest_handler).

-define(ID3AS_COMMON_USE_LOGGER, 1).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_common/include/id3as_types.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/rtmp.hrl").

-export([
         init/3,
         handle/1
        ]).

-define(state, ?MODULE).

-record(?state,
        {
         workflow_pid :: pid(),
         ingestStarted :: fun(),
         ingestStopped :: fun()
        }).

%% TODO - auth (will be config passed into rtmp_server), status

init(Rtmp, _ConnectArgs, [#{ingestStarted := IngestStarted,
                            ingestStopped := IngestStopped}]) ->

  {ok, State} = start_workflow(Rtmp),

  unit = ((IngestStarted(<<"stream1">>))(<<"high">>))(),

  {ok, State#?state{ingestStarted = IngestStarted,
                    ingestStopped = IngestStopped}}.

  %% {stop, rejected}.

handle(State = #?state{ingestStopped = IngestStopped}) ->

  %% the workflow is dealing with the RTMP, so just wait until it says we are done
  receive
    #workflow_output{message = #no_active_generators_msg{}} ->
      ?SLOG_WARNING("Client exited"),
      unit = ((IngestStopped(<<"stream1">>))(<<"high">>))(),
      ok;

    Other ->
      ?SLOG_WARNING("Unexpected workflow output ~p", [Other]),
      handle(State)
  end.

start_workflow(Rtmp) ->

  Workflow = #workflow{
                name = ingest,
                generators = [
                              #generator{name = rtmp_ingest,
                                         module = rtmp_push_ingest_generator,
                                         config = #rtmp_push_ingest_generator_config{
                                                     rtmp = Rtmp
                                                    }}
                             ],
                processors = [
                              #processor{name = demux,
                                         subscribes_to = rtmp_ingest,
                                         module = rtmp_tag_to_frame
                                        },

                              #processor{name = null,
                                         subscribes_to = ?previous,
                                         module = dev_null_processor}
                             ]
               },

  {ok, WorkflowPid} = id3as_workflow:start_link(Workflow),

  {ok, #?state{workflow_pid = WorkflowPid}}.
