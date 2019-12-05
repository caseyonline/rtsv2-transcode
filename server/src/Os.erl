-module(os@foreign).

-export([ getEnv_/3
        , osCmd/1
        , privCmd/1
        ]).

getEnv_(Nothing, Just, Name) when is_binary(Name) ->
  getEnv_(Nothing, Just, binary_to_list(Name));

getEnv_(Nothing, Just, Name) ->
  fun() ->
      case os:getenv(Name) of
        false ->
          Nothing;
        Value ->
          Just(list_to_binary(Value))
      end
  end.

privCmd(Command) ->
  fun() ->
      PrivDir = code:priv_dir(rtsv2),
      os:cmd(lists:append([PrivDir, "/", binary_to_list(Command)]))
  end.

osCmd(Command) -> fun() -> os:cmd(binary_to_list(Command)) end.
