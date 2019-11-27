-module(file@foreign).

-export([
         readFile_/3
        ]).

readFile_(Nothing, Just, Filename) ->
  fun () ->
      case file:read_file(Filename) of
        { ok, Binary } -> Just(Binary);
        _ -> Nothing
      end
  end.
