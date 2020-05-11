-module(gop_measurer).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").

-include("./gop_measurer.hrl").

-behaviour(workflow_processor).

-export([
         spec/1,
         initialise/1,
         process_input/2,
         flush/1,
         ioctl/2
        ]).

-define(state, ?MODULE).

-record(?state, {
  current_gop = #{} :: maps:map({stream_id(), atom()}, {integer(), pts()})
}).

%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{
     consumes = ?video_frames,
     generates = ?gop_measurements,
     supports_synchronous_mode = true,
     is_pure = true
    }.

initialise(_Processor = #processor{config = _Config}) ->
  {ok, #?state{}}.

process_input(#frame{gop_number = GopNumber,
                     pts = Pts,
                     stream_metadata = #stream_metadata{stream_id = StreamId},
                     source_metadata = #source_metadata{source_id = SourceId},
                     frame_metadata = #video_frame_metadata{ is_random_access_point = true },
                     profile = #video_profile{name = ProfileName}},
              State = #?state{ current_gop = GopMap }) ->
  Key = {StreamId, ProfileName},
  {GopMap2, Frames} = case maps:get(Key, GopMap, undefined) of
                        undefined ->
                          %% First frame for this key
                          { maps:put(Key, {GopNumber, Pts}, GopMap), [] };

                        {GopNumber, _LastPts} ->
                          %% Same gop; shouldn't really see this but no need to take out the workflow
                          { GopMap, [] };

                        {OldGopNumber, LastPts} when GopNumber =:= OldGopNumber + 1 ->
                          Output = #gop_measurement{ stream_id = StreamId, profile_name = ProfileName, source_id = SourceId, duration = Pts-LastPts },
                          { maps:put(Key, {GopNumber, Pts}, GopMap), [ Output ] };

                        {_OldGopNumber, _LastPts} ->
                          %% Skipped some gops - that can happen if the ingest stopped and restarted
                          { maps:put(Key, {GopNumber, Pts}, GopMap), [ ] }
                      end,

  {ok, Frames, State#?state{ current_gop = GopMap2 }};

process_input(_Frame, State = #?state{}) ->
  {ok, State}.

flush(State) ->
  {flush_complete, State}.

ioctl(_, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
