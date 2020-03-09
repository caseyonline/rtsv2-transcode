-module(rtsv2_handler_chaos@foreign).

-export([
         chaosImpl/1
        ]).

chaosImpl(#{ name := NameStr
           , num_hits := MNumHits
           , delay_between_hits_ms := MDelayMs }) ->
  fun() ->
      {ok, Tokens, _} = erl_scan:string(binary_to_list(NameStr)),
      {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
      {value, Name, _} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),

      NumHits = fromMaybe(1, MNumHits),

      DelayMs = fromMaybe(1, MDelayMs),

      Pid = case Name of
              {n, l, _} -> gproc:where(Name);
              _ -> whereis(Name)
            end,

      do_kill(Pid, NumHits, DelayMs),

      unit
  end.

do_kill(_Pid, 0, _DelayMs) ->
  ok;

do_kill(Pid, 1, _DelayMs) ->
  exit(Pid, kill),
  ok;

do_kill(Pid, NumHits, DelayMs) ->
  exit(Pid, kill),
  timer:sleep(DelayMs),
  do_kill(Pid, NumHits - 1, DelayMs).

fromMaybe(Default, {nothing}) ->
  Default;
fromMaybe(_Default, {just, Value}) ->
  Value.
