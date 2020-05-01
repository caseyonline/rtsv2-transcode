-module(rtsv2_handler_llnwStub@foreign).

-export([isAuthorized/1]).

isAuthorized(Req) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
      {basic, _User = <<"id3as">>, _Pwd = <<"id3as">>} ->
        true;
      _ -> false
  end.