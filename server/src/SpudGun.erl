-module(spudGun@foreign).

-export([post_/5, put_/5, get_/4]).

-include_lib("id3as_common/include/spud_gun.hrl").
-include_lib("id3as_common/include/common.hrl").

post_(Left, Right, Url, Body, UserHeaders) -> send_(post, Left, Right, Url, Body, UserHeaders).
put_(Left, Right, Url, Body, UserHeaders) -> send_(put, Left, Right, Url, Body, UserHeaders).

send_(Method, Left, Right, Url, Body, UserHeaders) -> fun() ->
    ReqHeaders = [ {<<"Accept-Encoding">>, <<"gzip">>} | UserHeaders ],
    Request = spud_gun:url_to_request_record(Url,
        #spud_request{request_timeout = 30000
                     ,headers = ReqHeaders
                     ,method = Method
                     ,body = Body }),
    case spud_gun:simple_request(Request) of
        {ok, StatusCode, Headers, RespBody} when StatusCode >= 200, StatusCode < 300 ->
            %% Gun lowercases response header names
            case proplists:lookup(<<"content-encoding">>, Headers) of
                {_, <<"gzip">>} -> Right(zlib:gunzip(RespBody));
                none -> Right(RespBody)
            end;
        {ok, _Other, _Headers, Body} -> Left(Body);
        Response ->
            ?DEBUG("spud gun ~p to ~p got response ~p", [Method, Url, Response]),
            Left(<<"request error">>)
    end
end.

get_(Nothing, Just, Url, ReqHeaders) -> fun() ->
    case spud_gun:get(Url, ReqHeaders) of
        {ok, StatusCode, _Headers, RespBody} when StatusCode >= 200, StatusCode < 300 -> Just(RespBody);
        Response ->
            io:format("spud gun get to ~p / ~p got response ~p~n", [Url, ReqHeaders, Response]),
            ?DEBUG("spud gun get to ~p got response ~p", [Url, Response]),
            Nothing
    end
end.
