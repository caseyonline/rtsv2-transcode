-module(rtsv2_nodeManager@foreign).

-export([
         getAgentKeys/1
        ]).

getAgentKeys(Agent) ->
  fun() ->
    ets:select(gproc, [{ {{{n,l,{Agent,'$1'}},n},'_','_'}, [], ['$1']}])
  end.
