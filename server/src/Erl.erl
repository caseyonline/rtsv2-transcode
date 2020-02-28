-module(erl_utils@foreign).

-export([ systemTimeImpl/1
        , vmTimeImpl/1
        , sleepImpl/1
        , makeRefImpl/0
        , privDirImpl/1
        , eqRefImpl/2
        , selfImpl/0
        , trapExitImpl/1
        ]).


systemTimeImpl(TimeUnit) ->
  fun() ->
      erlang:system_time(TimeUnit)
  end.

vmTimeImpl(TimeUnit) ->
  fun() ->
      erlang:monotonic_time(TimeUnit)
  end.

sleepImpl(Ms) ->
  fun() -> timer:sleep(Ms), unit end.

makeRefImpl() ->
  fun() -> make_ref() end.

privDirImpl(App) ->
  list_to_binary(code:priv_dir(App)).

eqRefImpl(Ref1, Ref2) ->
  Ref1 == Ref2.

selfImpl() ->
  fun() -> self() end.

trapExitImpl(Value) ->
  fun() ->
      process_flag(trap_exit, Value)
  end.
