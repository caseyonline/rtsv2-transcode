-module(logger@foreign).

-export([
         emergency_/2,
         alert_/2,
         critical_/2,
         error_/2,
         warning_/2,
         notice_/2,
         info_/2,
         debug_/2
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

emergency_(Msg, Args) ->
  fun() ->
      ?do_log(emergency, Msg, Args)
  end.

alert_(Msg, Args) ->
  fun() ->
      ?do_log(alert, Msg, Args)
  end.

critical_(Msg, Args) ->
  fun() ->
      ?do_log(critical, Msg, Args)
  end.

error_(Msg, Args) ->
  fun() ->
      ?do_log(error, Msg, Args)
  end.

warning_(Msg, Args) ->
  fun() ->
      ?do_log(warning, Msg, Args)
  end.

notice_(Msg, Args) ->
  fun() ->
      ?do_log(notice, Msg, Args)
  end.

info_(Msg, Args) ->
  fun() ->
      ?do_log(info, Msg, Args)
  end.

debug_(Msg, Args) ->
  fun() ->
      ?do_log(debug, Msg, Args)
  end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
