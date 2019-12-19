-module(rtsv2_config@foreign).

-include_lib("id3as_common/include/common.hrl").

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
      map_atom_keys_to_binaries(expand_environment_variables(Map))
  end.

expand_environment_variables(Map) when is_map(Map) ->
  maps:map(fun(_Key, Value) ->
               expand_environment_variables(Value)
           end,
           Map);

expand_environment_variables(List) when is_list(List) ->
  [expand_environment_variables(Item) || Item <- List];

expand_environment_variables(Binary) when is_binary(Binary) ->
  Parts = re:split(Binary, <<"(%[^%]*%)">>, [{return, binary}, group, trim]),

  Mapped = lists:map(fun([Prefix, Variable]) ->
                         case os:getenv(binary_to_list(binary:part(Variable, {1, size(Variable) - 2}))) of
                           false ->
                             [Prefix, Variable];
                           Value ->
                             [Prefix, list_to_binary(Value)]
                         end;
                        ([SingleMatch]) ->
                         SingleMatch
                     end,
                     Parts),

  iolist_to_binary(Mapped);

expand_environment_variables(Value) ->
  Value.

map_atom_keys_to_binaries(Map) when is_map(Map) ->
  maps:fold(fun(Key, Value, Acc) ->
                maps:put(atom_to_binary(Key, utf8), map_atom_keys_to_binaries(Value), Acc)
            end,
            #{},
            Map);

map_atom_keys_to_binaries(Value) ->
  Value.
