-module(rtsv2_rtmp_ingest_aggregator_processor).

-define(ID3AS_COMMON_USE_LOGGER, true).

-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").

-behaviour(workflow_processor).

-export([
         spec/1,
         initialise/1,
         process_input/2,
         flush/1,
         ioctl/2
        ]).

-define(state, ?MODULE).

-record(active_stream_state,
        {
         delta :: integer(),
         last_iframe_utc :: milliseconds(),
         last_iframe_dts :: ninety_khz_clock()
        }).

-record(?state,
        {
         permissible_delta_before_adjustment_ms = 250 :: milliseconds(),
         reference_stream :: undefined | term(),
         streams :: undefined | maps:map({term(), reference()}, #active_stream_state{})
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

process_input({ingest_stopped, _}, State = #?state{streams = undefined}) ->
  %% Ingest stopped before we saw any frames
  {ok, State};

process_input({ingest_stopped, Id}, State = #?state{reference_stream = ReferenceStream,
                                                    streams = Streams}) ->

  ?SLOG_INFO("Ingest stopped", #{stream => Id}),

  NewStreams = maps:filter(fun({Key, _Ref}, _) -> Key /= Id end, Streams),

  case ReferenceStream of
    {Id, _} ->
      case maps:fold(fun(Key, #active_stream_state{last_iframe_utc = LastIFrame}, undefined) ->
                         {LastIFrame, Key};
                        (Key, #active_stream_state{last_iframe_utc = LastIFrame}, Acc = {CandidateLastIFrame, _CandidateKey}) ->
                         if LastIFrame > CandidateLastIFrame -> {LastIFrame, Key};
                            ?otherwise -> Acc
                         end
                     end,
                     undefined,
                     NewStreams) of
        undefined ->
          ?SLOG_INFO("Ingest was reference.  No new reference selected", #{new_streams => NewStreams}),
          {ok, State#?state{reference_stream = undefined, streams = #{}}};

        {_, NewReferenceStream} ->
          ?SLOG_INFO("Ingest was reference.  New reference selected", #{reference => NewReferenceStream}),
          {ok, State#?state{reference_stream = NewReferenceStream,
                            streams = NewStreams}}
      end;
    _ ->
      {ok, State#?state{streams = NewStreams}}
  end;

process_input(Input = #frame{source_metadata = #source_metadata{source_id = Id,
                                                                source_instance = Instance},
                             frame_metadata = #video_frame_metadata{is_idr_frame = true},
                             vm_capture_us = CaptureUs,
                             dts = Dts}, State = #?state{reference_stream = undefined}) ->

  %% No reference - this stream is the new reference and no timestamp delta needed
  Key = {Id, Instance},

  ?SLOG_INFO("Stream selected as reference", #{stream => Id}),

  StreamState = #active_stream_state{delta = 0,
                                     last_iframe_utc = CaptureUs,
                                     last_iframe_dts = Dts},

  {ok, Input, State#?state{reference_stream = Key,
                           streams = maps:put(Key, StreamState, #{})}};

process_input(_Input, State = #?state{reference_stream = undefined}) ->
  %% We need an iframe to create a reference stream
  {ok, State};

process_input(Input = #frame{source_metadata = #source_metadata{source_id = Id,
                                                                source_instance = Instance},
                             frame_metadata = FrameMetadata,
                             vm_capture_us = CaptureUs,
                             pts = Pts,
                             dts = Dts}, State = #?state{streams = Streams,
                                                         reference_stream = ReferenceStream,
                                                         permissible_delta_before_adjustment_ms = PermissibleDelta}) ->

  Key = {Id, Instance},

  case maps:get(Key, Streams, undefined) of
    undefined ->
      %% We should also check if there's an entry for Id with an old instance (which could also be the reference - if the reference dies,
      %% then we need to promote one of the others (but keep its delta)

      case FrameMetadata of
        #video_frame_metadata{is_idr_frame = true} ->
          #active_stream_state{last_iframe_utc = ReferenceCaptureUs,
                               last_iframe_dts = ReferenceDts} = maps:get(ReferenceStream, Streams),

          %% We have two streams and a capture and dts for both
          %% If the capture and dts are sufficiently similar, then no delta needed - we assume that it's two synchronous streams from a single encoder
          %% However, if the timestamps differ too much, then we want to delta the non-reference stream to make it match the reference stream

          UsDelta = CaptureUs - ReferenceCaptureUs,
          DtsDelta = Dts - ReferenceDts,

          StreamDelta = if
                          abs(UsDelta) < (PermissibleDelta * 1000) andalso abs(DtsDelta) < (PermissibleDelta * 90) ->
                            %% These iframes are close enough - no delta needed
                            0;
                          ?otherwise ->
                            IdealDts = ReferenceDts + trunc(UsDelta * 0.09),
                            IdealDts - Dts
                        end,

          ?SLOG_INFO("Applying delta for stream", #{stream => Id,
                                                    reference => ReferenceStream,
                                                    delta => StreamDelta,
                                                    us_delta => UsDelta,
                                                    dts_delta => DtsDelta}),

          process_input(Input, State#?state{streams = maps:put(Key, #active_stream_state{delta = StreamDelta}, Streams)});

        _ ->
          %% We can't generate a reference delta on a non-iframe
          {ok, State}
      end;

    StreamState = #active_stream_state{delta = Delta} ->

      Output = Input#frame{pts = Pts + Delta,
                           dts = Dts + Delta},

      State2 = case FrameMetadata of
                 #video_frame_metadata{is_idr_frame = true} ->
                   StreamState2 = StreamState#active_stream_state{last_iframe_utc = CaptureUs,
                                                                  last_iframe_dts = Dts + Delta},

                   State#?state{streams = maps:put(Key, StreamState2, Streams)};
                 _ ->
                   State
               end,

      {ok, Output, State2}
  end.

flush(State) ->
  {flush_complete, State}.

ioctl(_, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
