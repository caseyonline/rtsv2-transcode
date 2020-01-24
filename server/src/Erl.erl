-module(erl_utils@foreign).

-export([ systemTimeImpl/1
        , sleepImpl/1
        , makeRefImpl/0
        ]).


systemTimeImpl(TimeUnit) ->
  fun() ->
      erlang:system_time(TimeUnit)
  end.

sleepImpl(Ms) ->
  fun() -> timer:sleep(Ms), unit end.

makeRefImpl() ->
  make_ref().
