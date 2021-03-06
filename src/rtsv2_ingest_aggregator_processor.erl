-module(rtsv2_ingest_aggregator_processor).

-include_lib("id3as_common/include/common.hrl").
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
         streams :: undefined | maps:map({term(), reference()}, #active_stream_state{}),
         pending_program_details = #{} :: maps:map(term(), frame()),
         audio_only :: boolean()
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

initialise(#processor{ config = AudioOnly }) ->
  {ok, #?state{ audio_only = AudioOnly }}.

process_input({ingest_stopped, _}, State = #?state{streams = undefined}) ->
  %% Ingest stopped before we saw any frames
  {ok, State};

process_input({ingest_stopped, Id = {ingestKey, _SlotId, _SlotRole, ProfileName}},
              State = #?state{reference_stream = ReferenceStream,
                              streams = Streams,
                              pending_program_details = PendingProgramDetails}) ->

  ?SLOG_INFO("Ingest stopped", #{stream => Id}),

  NewStreams = maps:filter(fun({Key, _Ref}, _) -> Key /= ProfileName end, Streams),

  PendingProgramDetails2 = maps:filter(fun(Key, _Frame) -> Key /= ProfileName end, PendingProgramDetails),

  case ReferenceStream of
    {ProfileName, _} ->
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
          {ok, State#?state{reference_stream = undefined,
                            streams = #{},
                            pending_program_details = PendingProgramDetails2
                           }};

        {_, NewReferenceStream} ->
          ?SLOG_INFO("Ingest was reference.  New reference selected", #{reference => NewReferenceStream}),
          {ok, State#?state{reference_stream = NewReferenceStream,
                            streams = NewStreams,
                            pending_program_details = PendingProgramDetails2
                           }}
      end;
    _ ->
      {ok, State#?state{streams = NewStreams,
                        pending_program_details = PendingProgramDetails2
                       }}
  end;

process_input(Input = #frame{frame_metadata = #video_frame_metadata{is_idr_frame = true}},
              State = #?state{reference_stream = undefined}) ->
  {Output, State2} = select_reference_stream(Input, State),
  {ok, Output, State2};
  
process_input(Input = #frame{type = audio},
              State = #?state{reference_stream = undefined,
                              audio_only = true}) ->
  {Output, State2} = select_reference_stream(Input, State),
  {ok, Output, State2};

process_input(Input = #frame{type = program_details,
                             source_metadata = #source_metadata{source_id = Id}},
              State = #?state{reference_stream = undefined,
                              pending_program_details = PendingProgramDetails}) ->
  %% We need an iframe to create a reference stream
  {ok, State#?state{pending_program_details = maps:put(Id, Input, PendingProgramDetails)}};

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
                                                         permissible_delta_before_adjustment_ms = PermissibleDelta,
                                                         pending_program_details = PendingProgramDetails,
                                                         audio_only = AudioOnly
                                                        }) ->

  Key = {Id, Instance},

  case maps:get(Key, Streams, undefined) of
    undefined ->
      %% We should also check if there's an entry for Id with an old instance (which could also be the reference - if the reference dies,
      %% then we need to promote one of the others (but keep its delta)

      case is_sync_frame(Input, AudioOnly) of
        true ->
          #active_stream_state{last_iframe_utc = ReferenceCaptureUs,
                               last_iframe_dts = ReferenceDts,
                               delta = ReferenceDelta
                              } = maps:get(ReferenceStream, Streams),

          %% We have two streams and a capture and dts for both
          %% If the capture and dts are sufficiently similar, then no delta needed - we assume that it's two synchronous streams from a single encoder
          %% However, if the timestamps differ too much, then we want to delta the non-reference stream to make it match the reference stream

          UsDelta = CaptureUs - ReferenceCaptureUs,
          DtsDelta = Dts - ReferenceDts,

          StreamDelta = ReferenceDelta + if
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

          {ok, DeltadFrame, State2} = process_input(Input, State#?state{streams = maps:put(Key, #active_stream_state{delta = StreamDelta}, Streams)}),

          Output = case maps:get(Id, PendingProgramDetails, undefined) of
                     undefined ->
                       DeltadFrame;
                     ProgramDetails ->
                       [ProgramDetails#frame{dts = DeltadFrame#frame.dts,
                                             pts = DeltadFrame#frame.dts}, DeltadFrame]
                   end,
          {ok, Output, State2#?state{pending_program_details = maps:remove(Id, PendingProgramDetails)}};

        false ->
          %% We can't generate a reference delta on a non-iframe
          case Input of
            #frame{type = program_details} ->
              {ok, State#?state{pending_program_details = maps:put(Id, Input, PendingProgramDetails)}};
            _ ->
              {ok, State}
          end
      end;

    StreamState = #active_stream_state{delta = Delta} ->

      Output = Input#frame{pts = Pts + Delta,
                           dts = Dts + Delta},
      State2 = case FrameMetadata of
                 #video_frame_metadata{is_idr_frame = true} ->
                   StreamState2 = StreamState#active_stream_state{last_iframe_utc = CaptureUs,
                                                                  last_iframe_dts = Dts},

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

is_sync_frame(#frame{ frame_metadata = #video_frame_metadata{is_idr_frame = true} }, _) -> true;
is_sync_frame(#frame{ type = audio }, _AudioOnly = true) -> true;
is_sync_frame(_, _) -> false.

select_reference_stream(Input = #frame{source_metadata = #source_metadata{source_id = Id,
                                                                          source_instance = Instance},
                                       vm_capture_us = CaptureUs,
                                       pts = Pts,
                                       dts = Dts},
                        State = #?state{pending_program_details = PendingProgramDetails}) ->
  %% No reference - this stream is the new reference and no timestamp delta needed
  Key = {Id, Instance},
  Now = ?now_ms * 90,
  Delta = Now - Dts,

  ?SLOG_INFO("Stream selected as reference", #{ stream => Id
                                              , delta => Delta}),

  StreamState = #active_stream_state{delta = Delta,
                                     last_iframe_utc = CaptureUs,
                                     last_iframe_dts = Dts},

  DeltadFrame = Input#frame{pts = Pts + Delta,
                            dts = Dts + Delta
                           },

  Output = case maps:get(Id, PendingProgramDetails, undefined) of
             undefined ->
               DeltadFrame;
             ProgramDetails ->
               [ProgramDetails#frame{pts = Dts + Delta, dts = Dts + Delta}, DeltadFrame]
           end,

  {Output, State#?state{reference_stream = Key,
                        streams = maps:put(Key, StreamState, #{}),
                        pending_program_details = maps:remove(Id, PendingProgramDetails)
                        }}.