-module(erl_utils@foreign).

-export([ systemTimeImpl/1
        , sleepImpl/1
        , makeRefImpl/0
        , privDirImpl/1
        , eqRefImpl/2
        ]).


systemTimeImpl(TimeUnit) ->
  fun() ->
      erlang:system_time(TimeUnit)
  end.

sleepImpl(Ms) ->
  fun() -> timer:sleep(Ms), unit end.

makeRefImpl() ->
  fun() -> make_ref() end.

privDirImpl(App) ->
  list_to_binary(code:priv_dir(App)).


eqRefImpl(Ref1, Ref2) ->
  Ref1 == Ref2.
