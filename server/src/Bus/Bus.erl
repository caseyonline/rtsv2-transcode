-module(bus@foreign).

-export([
         subscribe_/3,
         unsubscribe_/1,
         raise_/2]).

-include_lib("id3as_common/include/id3as_message_bus.hrl").

%% This is very much like the code in the pubsub stuff itself anyway...
%% TODO: Get steve to check we're not losing pids
subscribe_(Ref, Bus, Mapper) ->
  Recipient = self(),
  Fun = fun Fun(MaybeMonitorRef) ->
            MonitorRef = case MaybeMonitorRef of
                           undefined ->
                             ?I_SUBSCRIBE_BUS_MSGS(Bus),
                             monitor(process, Recipient);
                           _ -> MaybeMonitorRef
                         end,
            receive
              stop ->
                demonitor(MonitorRef),
                ok;
              {'DOWN', _, _, _, _} -> ok;
              Msg ->
                Recipient ! Mapper(Msg),
                Fun(MonitorRef)
            end
        end,
  fun() ->
      Pid = spawn_link(fun() -> Fun(undefined) end),
      Ref(Pid)
  end.

unsubscribe_(Pid) ->
  fun() ->
      Pid ! stop,
      ok
  end.

raise_(Bus, Msg) ->
  fun() ->
      ?I_RAISE_BUS_MSG(Bus, Msg)
  end.
