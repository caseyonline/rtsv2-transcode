-module(erl_utils@foreign).

-export([
         isRegisteredImpl/1,
         systemTimeImpl/1
        ]).

isRegisteredImpl(Name) ->
  fun() ->
      case whereis(Name) of
        undefined ->
          false;
        _ ->
          true
      end
  end.

systemTimeImpl(TimeUnit) ->
  fun() ->
      erlang:system_time(TimeUnit)
  end.
