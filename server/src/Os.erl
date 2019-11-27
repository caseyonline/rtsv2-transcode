-module(os@foreign).

-export([
         getEnv_/3
        ]).

getEnv_(Nothing, Just, Name) ->
  fun() ->
      case os:getenv(Name) of
        false ->
          Nothing;
        Value ->
          Just(Value)
      end
  end.
