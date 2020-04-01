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

-define(do_effectful_log(Level, Msg, Args),
        [{current_stacktrace, Stack}] = erlang:process_info(self(), [current_stacktrace]),
        fun() ->
            {Module, Fun, Arity, File, Line} = walk_stack(Stack),

            Location = #{mfa => {Module, Fun, Arity},
                         module => Module,
                         line => Line,
                         file => File},

            case logger:allow(Level, Module) of
              true ->
                apply(logger, macro_log, [Location, Level, binary_to_list(Msg), [], Args]),
                unit;
              false ->
                unit
            end
        end).

emergency(Msg, Args) ->
  ?do_effectful_log(emergency, Msg, Args).

alert(Msg, Args) ->
  ?do_effectful_log(alert, Msg, Args).

critical(Msg, Args) ->
  ?do_effectful_log(critical, Msg, Args).

error(Msg, Args) ->
  ?do_effectful_log(error, Msg, Args).

warning(Msg, Args) ->
  ?do_effectful_log(warning, Msg, Args).

notice(Msg, Args) ->
  ?do_effectful_log(notice, Msg, Args).

info(Msg, Args) ->
  ?do_effectful_log(info, Msg, Args).

debug(Msg, Args) ->
  ?do_effectful_log(debug, Msg, Args).

spyImpl(Msg, Args) ->
  ?do_effectful_log(notice, Msg, Args).

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------
walk_stack([_LoggerFrame | Stack = [{TopModule, TopFun, TopArity, [{file, TopFile}, {line, TopLine}]} | _]]) ->
  walk_stack_internal({TopModule, TopFun, TopArity, TopFile, TopLine}, Stack).

walk_stack_internal(Default, [{Module, Fun, Arity, [{file, File}, {line, Line}]} | Rest]) ->
  ModuleStr = atom_to_list(Module),
  case string:prefix(ModuleStr, "logger@ps") of
    nomatch ->
      case string:find(ModuleStr, "@ps") of
        "@ps" ->
          {format(ModuleStr), Fun, Arity, File, Line};
        _ ->
          walk_stack_internal(Default, Rest)
      end;
    _ ->
      walk_stack_internal(Default, Rest)
  end;

walk_stack_internal(Default, []) -> Default.

format(Str) ->
  list_to_atom(string:join([camel(Token) || Token <- string:tokens(Str, "_")], ".")).

camel([H | T]) when $a =< H, H =< $z ->
  [H - 32 | T];
camel(Other) ->
  Other.
