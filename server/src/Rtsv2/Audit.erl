-module(rtsv2_audit@foreign).

-export([
         toList/1
        ]).

toList(Binary) when is_binary(Binary) ->
  binary_to_list(Binary).
