-module(rtsv2_agents_transPoP@foreign).

-export([ to_binary/1
        ]).

to_binary(X) ->
  term_to_binary(X).
