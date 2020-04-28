-module(rtsv2_load@foreign).

-export([
         cpuUtilInitImpl/0,
         cpuUtilImpl/1,
         networkUtilInitImpl/0,
         networkUtilImpl/1,
         schedUtilInitImpl/0,
         schedUtilImpl/1
        ]).

cpuUtilInitImpl() ->
  fun() ->
      ok
  end.

cpuUtilImpl(State) ->
  fun() ->
      Util = cpu_sup:util(),
      {Util * 100, State}
  end.

networkUtilInitImpl() ->
  fun() ->
      ok
  end.

networkUtilImpl(State) ->
  fun() ->
      {0, State}
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
