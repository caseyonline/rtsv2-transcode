-module(rtsv2_handler_chaos@foreign).

-export([
         chaosImpl/1
        ]).

chaosImpl(#{ name := NameStr
           , num_hits := NumHits
           , delay_between_hits_ms := DelayMs }) ->
  fun() ->
      {ok, Tokens, _} = erl_scan:string(binary_to_list(NameStr)),
      {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
      {value, Name, _} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),

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
