-module(ip@foreign).

-export([
         getInterfaceIp_/3
        ]).

getInterfaceIp_(Nothing, Just, Name) when is_binary(Name) ->
  fun() ->
      case ip_utils:get_interface_ip(binary_to_atom(Name, utf8)) of
        undefined ->
          Nothing;
        {O1, O2, O3, O4} ->
          Just({ipv4, O1, O2, O3, O4})
      end
  end.
