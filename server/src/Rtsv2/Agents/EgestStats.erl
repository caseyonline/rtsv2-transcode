-module(rtsv2_agents_egestStats@foreign).

-export([
         getEgestKeys/0
        ]).

getEgestKeys() ->
  fun() ->
    ets:select(gproc, [{ {{{n,l,{<<"Egest">>,'$1'}},n},'_','_'}, [], ['$1']}])
  end.
