-module(logger@foreign).

-export([
         emergency/2,
         alert/2,
         critical/2,
         error/2,
         warning/2,
         notice/2,
         info/2,
         debug/2,
         spyImpl/2
        ]).

-define(do_log(Level, Msg, Args),
        [{current_stacktrace, [_LoggerFrame, {Module, Fun, Arity, [{file, File}, {line, Line}]} | _]}] = erlang:process_info(self(), [current_stacktrace]),

        Location = #{mfa=> {Module, Fun, Arity},
                     line=> Line,
                     file=> File},

        case logger:allow(Level, Module) of
          true ->
            apply(logger, macro_log, [Location, Level, binary_to_list(Msg), [], Args]);
          false ->
            ok
        end).

emergency(Msg, Args) ->
  fun() ->
      ?do_log(emergency, Msg, Args)
  end.

alert(Msg, Args) ->
  fun() ->
      ?do_log(alert, Msg, Args)
  end.

critical(Msg, Args) ->
  fun() ->
      ?do_log(critical, Msg, Args)
  end.

error(Msg, Args) ->
  fun() ->
      ?do_log(error, Msg, Args)
  end.

warning(Msg, Args) ->
  fun() ->
      ?do_log(warning, Msg, Args)
  end.

notice(Msg, Args) ->
  fun() ->
      ?do_log(notice, Msg, Args)
  end.

info(Msg, Args) ->
  fun() ->
      ?do_log(info, Msg, Args)
  end.

debug(Msg, Args) ->
  fun() ->
      ?do_log(debug, Msg, Args)
  end.

spyImpl(Msg, Args) ->
  fun() ->
      ?do_log(notice, Msg, Args)
  end.
