-module(rtsv2_app@foreign).

-export([
         setLogRoot/0
        ]).

setLogRoot() ->
  fun() ->
      Root = gproc:get_env(l, rtsv2, disk_log_root, [os_env, app_env, {default, "logs"}]),

      Handlers = application:get_env(rtsv2, disk_log_handlers, []),

      lists:foreach(fun({handler, Id, Module, HandlerConfig = #{config := Config = #{file := File}}}) ->

                        Config2 = maps:put(file, filename:join(Root, File), Config),
                        HandlerConfig2 = maps:put(config, Config2, HandlerConfig),

                        logger:add_handler(Id, Module, HandlerConfig2)
                    end,
                    Handlers),
      ok
  end.
