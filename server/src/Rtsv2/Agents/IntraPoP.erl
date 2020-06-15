-module(rtsv2_agents_intraPoP@foreign).

-export([ to_binary/1
        , from_binary/1
        ]).

to_binary(X) ->
  term_to_binary(X).

from_binary(X) ->
  binary_to_term(X).
