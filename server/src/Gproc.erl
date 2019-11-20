-module(gproc@foreign).

-export([ registered_/1
        , register_/1
        ]).

registered_(Name) ->
  fun() -> gproc:where({n, l, Name}) /= undefined end.

register_(Name) ->
  fun() -> gproc:reg({n, l, Name}) end.
