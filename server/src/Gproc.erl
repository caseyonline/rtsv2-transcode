-module(gproc@foreign).

-export([
         registered_/1
        ]).

registered_(Name) ->
  fun() -> gproc:where({n, l, Name}) /= undefined end.
