-module(rtsv2_load@foreign).

-export([
         cpuUtilInitImpl/0,
         cpuUtilImpl/1,
         networkUtilInitImpl/0,
         networkUtilImpl/1,
         schedUtilInitImpl/0,
         schedUtilImpl/1
        ]).

-include_lib("id3as_common/include/common.hrl").

cpuUtilInitImpl() ->
  fun() ->
      ok
  end.

cpuUtilImpl(State) ->
  fun() ->
      Util = cpu_sup:util(),
      {Util, State}
  end.

networkUtilInitImpl() ->
  fun() ->
      case os:type() of
        {unix, darwin} -> undefined;
        _ ->
          {Receive, Transmit} = read_proc_dev_net(),
          queue:in({Receive + Transmit, ?vm_now_ms}, queue:new())
      end
  end.

networkUtilImpl(undefined) ->
  fun() ->
      {{just, 0}, undefined}
  end;

networkUtilImpl(Queue) ->
  fun() ->
      {Receive, Transmit} = read_proc_dev_net(),
      Now = ?vm_now_ms,
      Queue2 = queue:in({Receive + Transmit, Now}, Queue),
      Queue3 = expire_entries(Now - 10000, Queue2),
      RollingBitrate = bitrate(Queue3),
      {case RollingBitrate of
         undefined -> {nothing};
         _ -> {just, RollingBitrate}
       end, Queue3}
  end.

schedUtilInitImpl() ->
  fun() ->
      erlang:system_flag(scheduler_wall_time, true),
      {Cores, Threads} = case os:type() of
                           {unix, darwin} -> {1, 2};
                           _ ->
                             lists:foldl(fun({processor, Cores}, {CoreAcc, ThreadAcc}) ->
                                             {CoreAcc + length(Cores), lists:foldl(fun({core, Threads}, Acc) ->
                                                                                       Acc + length(Threads)
                                                                                   end,
                                                                                   ThreadAcc,
                                                                                   Cores)}
                                         end,
                                         {0, 0},
                                         erlang:system_info(cpu_topology))
                         end,
      Ts1 = lists:sort(erlang:statistics(scheduler_wall_time)),
      {Ts1, Threads / Cores}
  end.

schedUtilImpl({TsPrevious, CoreFactor}) ->
  fun() ->
      TsNew = lists:sort(erlang:statistics(scheduler_wall_time)),
      {A, T} = lists:foldl(fun({{_, A0, T0}, {_, A1, T1}}, {Ai,Ti}) ->
                               {Ai + (A1 - A0), Ti + (T1 - T0)} end,
                           {0, 0},
                           lists:zip(TsPrevious,TsNew)),

      {A * 100 / T * CoreFactor, TsNew}
  end.

read_proc_dev_net() ->
  {ok, B} = file:read_file("/proc/net/dev"),
  LinesWithHeader = binary:split(B, <<"\n">>, [global]),
  Lines = tl(tl(LinesWithHeader)),
  sum_lines(Lines, 0, 0).

sum_lines([], Receive, Transmit) ->
  {Receive, Transmit};

sum_lines([H | T], Receive, Transmit) ->
  case binary:split(H, <<" ">>, [global, trim_all]) of
    [ IFace
    , BytesReceived, _PacketsReceived, _ErrsReceived, _DropReceived, _FifoReceived, _FrameReceived, _CompressedReceived, _MulticastReceived
    , BytesTransmit, _PacketsTransmit, _ErrsTransmit, _DropTransmit, _FifoTransmit, _CollsTransmit, _CarrierTransmit, _CompressedTransmit] ->

      case IFace of
        <<"lo:">> ->
          sum_lines(T, Receive, Transmit);
        _ ->
          sum_lines(T, Receive + binary_to_integer(BytesReceived), Transmit + binary_to_integer(BytesTransmit))
      end;

    [] ->
      sum_lines(T, Receive, Transmit)
  end.

expire_entries(Expiry, Queue) ->
  case queue:out(Queue) of
    {{value, {_Bitrate, When}}, Queue2} when When < Expiry -> expire_entries(Expiry, Queue2);
    _ -> Queue
  end.

bitrate(Queue) ->
  case queue:peek(Queue) of
    {value, {OldestBytes, OldestTime}} ->
      case queue:peek_r(Queue) of
        {value, {NewestBytes, NewestTime}} when NewestTime > OldestTime ->
          ((NewestBytes - OldestBytes) * 8) / (NewestTime - OldestTime);
        _ ->
          undefined
      end;
    _ ->
      undefined

  end.
