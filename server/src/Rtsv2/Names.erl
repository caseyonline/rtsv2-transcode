-module(rtsv2_names@foreign).

-export([
         viaIsRegisteredImpl/2
        ]).

viaIsRegisteredImpl(Module, Name) ->
  fun() -> Module:whereis_name(Name) /= undefined end.
