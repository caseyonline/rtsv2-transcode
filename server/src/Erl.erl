-module(erl_utils@foreign).

-export([ systemTimeImpl/1
        , vmTimeImpl/1
        , sleepImpl/1
        , makeRefImpl/0
        , privDirImpl/1
        , eqRefImpl/2
        , selfImpl/0
        , monitorImpl/1
        , trapExitImpl/1
        , mapExitReasonImpl/1
        , exitMessageMapperImpl/1
        , shutdownImpl/1
        , refToStringImpl/1
        , stringToRefImpl/1
        , exitImpl/1
        , readTuple2Impl/1
        , base64EncodeImpl/1
        , linkImpl/1
        , whereisImpl/1
        , monotonicTime/0
        ]).

linkImpl(Pid) ->
  fun() ->
      link(Pid)
  end.

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

whereisImpl(Target) ->
  fun() ->
      Pid = case Target of
              P when is_pid(P) ->
                P;
              {via, Module, Name} ->
                Module:whereis_name(Name);
              {global, Name} ->
                global:whereis_name(Name);
              Name when is_atom(Name) ->
                whereis(Name)
            end,

      case Pid of
        undefined -> {nothing};
        _ -> {just, Pid}
      end
  end.

monitorImpl(ToMonitor) ->
  fun() ->
      Pid = case ToMonitor of
              P when is_pid(P) ->
                P;
              {via, Module, Name} ->
                Module:whereis_name(Name);
              {global, Name} ->
                global:whereis_name(Name);
              Name when is_atom(Name) ->
                whereis(Name)
            end,

      case Pid of
        undefined ->
          %% DOWN
          Ref = make_ref(),
          self() ! {'DOWN', Ref, process, undefined, noproc};
        _ ->
          erlang:monitor(process, Pid)
      end
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

readTuple2Impl({A, B}) ->
  {just, {A, B}};
readTuple2Impl(_) ->
  {nothing}.

base64EncodeImpl(String) ->
  base64:encode(String).

monotonicTime() ->
  fun() ->
      erlang:monotonic_time()
  end.
