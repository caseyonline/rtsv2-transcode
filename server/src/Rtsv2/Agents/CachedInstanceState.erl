-module(rtsv2_agents_cachedInstanceState@foreign).

-export([
         receiveImpl/1
        ]).

receiveImpl(Ref) ->
  fun() ->
      receive
        Ref -> ok
      end
  end.
