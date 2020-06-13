-module(rtsv2_agents_cachedInstanceState@foreign).

-include_lib("id3as_common/include/common.hrl").

-behaviour(gen_server).

%% FFIs
-export([
         receiveImpl/1,
         spawnChild/4
        ]).

receiveImpl(Ref) ->
  fun() ->
      receive
        Ref -> ok
      end
  end.

spawnChild(Parent, Caller, Ref, ChildStartLink) ->
  fun() ->
      spawn_link(fun() ->
                     erlang:process_flag(trap_exit, true),

                     Res = ChildStartLink(),

                     %% Tell the original caller to CachedInstanceState:start_link the result that matters
                     Caller ! Ref,
                     case Res of
                       {ok, Pid} ->
                         receive
                           {'EXIT', Parent, Reason} ->
                             gen_server:stop(Pid, Reason, 5000),
                             exit(Reason);

                           {'EXIT', _Child, Reason} ->
                             exit(Reason)
                         end;
                       _Other ->
                         ok
                     end
                 end)
  end.
