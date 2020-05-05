-module(rtsv2_sup@foreign).

-export([
         traceDebugImpl/0
        ]).

-include_lib("id3as_common/include/common.hrl").

traceDebugImpl() ->
  fun() ->
      lists:foreach(fun(Path) -> ?INFO("PATH: ~p", [Path]) end, code:get_path()),
      lists:foreach(fun(Module) -> ?INFO("MOD: ~p", [Module]) end, code:all_loaded()),
      unit
  end.
