-module(rtp_timestamp_wrap).


-export([ new/1
        , compute_extended_timestamp/2
        ]).


-record(rtc_timestamp_rollover_state,
        { high_bits = 0 :: non_neg_integer()
        , highest_timestamp :: non_neg_integer()
        }).


-type state() :: {rtc_timestamp_rollover_initial,integer()} | #rtc_timestamp_rollover_state{}.
-opaque_type([state/0]).

-type timestamp() :: non_neg_integer().
-type extended_timestamp() :: non_neg_integer().
-type high_bits() :: non_neg_integer().
-export_type([ timestamp/0
             , extended_timestamp/0
             ]).


-spec new(integer()) -> state().
new(ClockRate) ->
  {rtc_timestamp_rollover_initial,ClockRate}.

-spec compute_extended_timestamp(state(), timestamp()) -> {state(), high_bits(), extended_timestamp()}.
compute_extended_timestamp(State, Timestamp) ->
  {NewState = #rtc_timestamp_rollover_state{high_bits = ROC}, PacketIndex} = compute_extended_timestamp_prime(State, Timestamp),
  {NewState, ROC, PacketIndex}.

compute_extended_timestamp_prime({rtc_timestamp_rollover_initial,ClockRate}, Timestamp) ->
  CurrentTimestamp = i_timestamp:to_epoch_milliseconds(i_timestamp:utc_now()),
  MediaClock = round(CurrentTimestamp * ClockRate / 1000),
  HighBits = MediaClock bsr 32,
  {#rtc_timestamp_rollover_state{highest_timestamp = Timestamp, high_bits = HighBits}, build_extended_timestamp(HighBits, Timestamp)};

compute_extended_timestamp_prime(State = #rtc_timestamp_rollover_state{high_bits = ROC, highest_timestamp = HighestTimestamp}, Timestamp) ->

  HighestIndex = build_extended_timestamp(ROC, HighestTimestamp),

  {IndexInLastWindow, DistOfIndexInLastWindow} = measure_timestamp_distance(HighestIndex, ROC - 1, Timestamp),
  {IndexInThisWindow, DistOfIndexInThisWindow} = measure_timestamp_distance(HighestIndex, ROC, Timestamp),
  {IndexInNextWindow, DistOfIndexInNextWindow} = measure_timestamp_distance(HighestIndex, ROC + 1, Timestamp),

  case smallest(DistOfIndexInLastWindow, DistOfIndexInThisWindow, DistOfIndexInNextWindow) of
    first ->
      {State, IndexInLastWindow};
    second ->
      NewState = State#rtc_timestamp_rollover_state{ highest_timestamp = max(Timestamp, HighestTimestamp) },
      {NewState, IndexInThisWindow};
    third ->
      NewState = State#rtc_timestamp_rollover_state{ high_bits = ROC + 1
                                                   , highest_timestamp = Timestamp
                                                   },
      {NewState, IndexInNextWindow}
    end.

%%% ============================================================================
%%% Private Functions
%%% ============================================================================
measure_timestamp_distance(_HighestIndex, _HighBits = -1, _Timestamp) ->
  {undefined, undefined};

measure_timestamp_distance(HighestIndex, HighBits, Timestamp) ->
  PacketIndex = build_extended_timestamp(HighBits, Timestamp),
  {PacketIndex, abs(HighestIndex - PacketIndex)}.


build_extended_timestamp(HighBits, Timestamp) ->
  (HighBits bsl 32) bxor Timestamp.


smallest(First, Second, Third) when First =< Second andalso First =< Third ->
  first;
smallest(_First, Second, Third) when Second =< Third ->
  second;
smallest(_First, _Second, _Third) ->
  third.
