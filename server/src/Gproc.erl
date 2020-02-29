-module(gproc@foreign).

-export([ registered_/1
        , register_/1
        , whereis_/1
        , match_/1
        ]).

registered_(Name) ->
  fun() -> gproc:where({n, l, Name}) /= undefined end.

register_(Name) ->
  fun() -> gproc:reg({n, l, Name}) end.

whereis_(Name) ->
  fun() -> case gproc:where({n, l, Name}) of
             undefined -> {left, unit};
             Pid -> {right, Pid}
           end
  end.

match_(MatchSpec) ->
  fun() -> [{via, gproc, element(1, element(1, E))}
            || E <- ets:select(gproc, [{ {{{n,l,MatchSpec},n},'_','_'}, [], ['$_']}])]
  end.
