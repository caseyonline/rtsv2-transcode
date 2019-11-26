-module(rtsv2_config@foreign).

-export([
          getEnv_/1,
          getMap_/1
        ]).


getEnv_(Name) ->
  fun() ->
      gproc:get_env(l, rtsv2, Name, [os_env, app_env, error])
  end.

getMap_(Name) ->
  fun() ->
      Map = gproc:get_env(l, rtsv2, Name, [os_env, app_env, error]),
      map_atom_keys_to_binaries(Map)
  end.


map_atom_keys_to_binaries(Map) when is_map(Map) ->
  maps:fold(fun(Key, Value, Acc) ->
                maps:put(atom_to_binary(Key, utf8), map_atom_keys_to_binaries(Value), Acc)
            end,
            #{},
            Map);

map_atom_keys_to_binaries(Value) ->
  Value.
