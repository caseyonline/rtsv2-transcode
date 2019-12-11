-module(erl_utils@foreign).

-export([ isRegisteredImpl/1
        , systemTimeImpl/1
        , sleepImpl/1
        , makeRefImpl/0
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

sleepImpl(Ms) ->
  fun() -> timer:sleep(Ms), unit end.

makeRefImpl() ->
  make_ref().
