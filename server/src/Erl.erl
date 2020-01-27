-module(erl_utils@foreign).

-export([ systemTimeImpl/1
        , sleepImpl/1
        , makeRefImpl/0
        , privDirImpl/1
        ]).


systemTimeImpl(TimeUnit) ->
  fun() ->
      erlang:system_time(TimeUnit)
  end.

sleepImpl(Ms) ->
  fun() -> timer:sleep(Ms), unit end.

makeRefImpl() ->
  make_ref().

privDirImpl(App) ->
  list_to_binary(code:priv_dir(App)).
