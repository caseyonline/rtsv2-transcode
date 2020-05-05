-module(rtsv2_ingest_qos_monitor).

-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").

-include("./rtsv2_ingest_qos_monitor.hrl").

-behaviour(workflow_processor).

-export([
         spec/1,
         initialise/1,
         process_input/2,
         handle_info/2,
         flush/1,
         ioctl/2
        ]).

-define(state, ?MODULE).
-record(?state,
        { poll_interval_ms :: milliseconds()
        , abort_if_no_media_in_ms :: milliseconds()
        , last_frame_seen :: milliseconds()
        }).

%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{
     consumes = ?frames,
     supports_synchronous_mode = true
    }.

initialise(_Processor = #processor{config = #rtsv2_ingest_qos_monitor_config{ poll_interval_ms = PollInterval
                                                                            , abort_if_no_media_in_ms = AbortMs
                                                                            }}) ->
  erlang:send_after(PollInterval, self(), poll),

  {ok, #?state{ poll_interval_ms = PollInterval
              , abort_if_no_media_in_ms = AbortMs
              , last_frame_seen = ?vm_now_ms
              }}.

process_input(#frame{}, State) ->
  {ok, State#?state{last_frame_seen = ?vm_now_ms}};

process_input(_, State) ->
  {ok, State}.

handle_info(poll, State = #?state{ poll_interval_ms = PollInterval
                                 , abort_if_no_media_in_ms = AbortMs
                                 , last_frame_seen = LastSeen
                                 }) ->
  Now = ?vm_now_ms,

  if
    Now - LastSeen > AbortMs ->
      ?SLOG_WARNING("Ingest workflow abort since no media seen", #{abort_ms => AbortMs}),
      {stop, {error, nomedia}, State};

    ?otherwise ->
      erlang:send_after(PollInterval, self(), poll),
      {noreply, State}
  end.

flush(State) ->
  {flush_complete, State}.

ioctl(_, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
