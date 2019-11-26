-module(rtsv2_config@foreign).

-export([ getEnv_/1
        ]).


getEnv_(Name) ->
  fun() ->
      gproc:get_env(l, rtsv2, Name, [os_env, app_env, error])
  end.
