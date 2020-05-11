-module(rtmp_onfi_to_frame).

-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/rtmp.hrl").

-behaviour(workflow_processor).

-export([
         spec/1,
         initialise/1,
         process_input/2,
         flush/1,
         ioctl/2
        ]).

-define(state, ?MODULE).
-record(?state,
        {
         last_pts :: undefined | ninety_khz_clock()
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

process_input(_Message = #rtmp_onfi_message{}, State = #?state{last_pts = undefined}) ->
  {ok, State};

process_input(Message = #rtmp_onfi_message{}, State = #?state{last_pts = LastPts}) ->

  Frame = #frame{type = script_frame,
                 frame_metadata = Message,
                 pts = LastPts,
                 dts = LastPts
                },

  {ok, Frame, State};

process_input(Frame = #frame{pts = Pts}, State) ->
  {ok, Frame, State#?state{last_pts = Pts}};

process_input(Other, State) ->
  {ok, Other, State}.

flush(State) ->
  {flush_complete, State}.

ioctl(_, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
