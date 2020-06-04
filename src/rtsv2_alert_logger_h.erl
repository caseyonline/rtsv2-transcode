-module(rtsv2_alert_logger_h).

-include_lib("id3as_common/include/id3as_message_bus.hrl").

-export([ log/2
        , adding_handler/1
        , changing_config/3
        ]).

log(LogEvent, _Config = #{config := #{message_bus := Bus}}) ->
  try
    ?I_RAISE_BUS_MSG(Bus, LogEvent)
  catch
    Class:Reason ->
      io:format(user, "CRASH ~p / ~p~n", [Class, Reason]),
      ok
  end,
  ok.

adding_handler(Config) ->
  erl_logger_filters:preprocess_config(Config).

changing_config(_SetOrUpdate, _Old, Config) ->
  erl_logger_filters:preprocess_config(Config).
