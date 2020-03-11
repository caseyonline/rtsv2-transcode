-module(erl_utils@foreign).

-export([ systemTimeImpl/1
        , vmTimeImpl/1
        , sleepImpl/1
        , makeRefImpl/0
        , privDirImpl/1
        , eqRefImpl/2
        , selfImpl/0
        , trapExitImpl/1
        , mapExitReasonImpl/1
        , exitMessageMapperImpl/1
        , shutdownImpl/1
        , refToStringImpl/1
        , stringToRefImpl/1
        , exitImpl/1
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

mapExitReasonImpl(normal) -> {normal};
mapExitReasonImpl({shutdown, Term}) -> {shutdown, Term};
mapExitReasonImpl(Other) -> {other, Other}.

exitMessageMapperImpl({'EXIT', Pid, Reason}) -> {just, {exit, Pid, Reason}};
exitMessageMapperImpl(_) -> {nothing}.

shutdownImpl(Pid) ->
  fun() ->
      i_utils:shutdown(Pid)
  end.

refToStringImpl(Ref) ->
  RefStr = erlang:ref_to_list(Ref),
  RefNum = list_to_binary(string:sub_string(RefStr, 6, length(RefStr) - 1)),
  RefNum.

stringToRefImpl(RefNum) ->
  try
    Ref = list_to_ref("#Ref<" ++ binary_to_list(RefNum) ++ ">"),
    {just, Ref}
  catch
    error:badarg ->
      {nothing}
  end.

exitImpl(Reason) ->
  fun() ->
      exit(Reason)
  end.
