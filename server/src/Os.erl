-module(os@foreign).

-export([
         getEnv_/3
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
