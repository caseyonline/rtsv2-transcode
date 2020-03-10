-module(rtsv2_handler_chaos@foreign).

-export([
         chaosImpl/2
        ]).

chaosImpl(#{ exit_reason := MExitReason
           , num_hits := MNumHits
           , delay_between_hits_ms := MDelayMs }, Name) ->
  fun() ->
      %% {ok, Tokens, _} = erl_scan:string(binary_to_list(NameStr)),
      %% {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
      %% {value, Name, _} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),

      ExitReason = binary_to_atom(fromMaybe(<<"exit">>, MExitReason), utf8),
      NumHits = fromMaybe(1, MNumHits),
      DelayMs = fromMaybe(1, MDelayMs),

      Pid = case Name of
              {n, l, _} -> gproc:where(Name);
              _ -> whereis(Name)
            end,

      do_kill(Pid, ExitReason, NumHits, DelayMs),

      unit
  end.

do_kill(_Pid, _ExitReason, 0, _DelayMs) ->
  ok;

do_kill(Pid, ExitReason, 1, _DelayMs) ->
  exit(Pid, ExitReason),
  ok;

do_kill(Pid, ExitReason, NumHits, DelayMs) ->
  exit(Pid, ExitReason),
  timer:sleep(DelayMs),
  do_kill(Pid, ExitReason, NumHits - 1, DelayMs).

fromMaybe(Default, {nothing}) ->
  Default;
fromMaybe(_Default, {just, Value}) ->
  Value.
