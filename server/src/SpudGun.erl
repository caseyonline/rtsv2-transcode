-module(spudGun@foreign).

-export([
         getImpl/4,
         putImpl/5,
         postImpl/5,
         deleteImpl/4
        ]).

-include_lib("id3as_common/include/spud_gun.hrl").
-include_lib("id3as_common/include/common.hrl").

getImpl(Left, Right, Url, ReqHeaders) ->
  fun() ->
      case spud_gun:get(Url, ReqHeaders) of
        {ok, StatusCode, _Headers, RespBody} when StatusCode >= 200, StatusCode < 300 ->
          Right(RespBody);
        Response ->
          ?SLOG_DEBUG("spud gun get failed", #{url => Url,
                                               response => Response}),
          Left(<<"get error">>)
      end
  end.

putImpl(Left, Right, Url, Body, UserHeaders) ->
  sendImpl(put, Left, Right, Url, Body, UserHeaders).

postImpl(Left, Right, Url, Body, UserHeaders) ->
  sendImpl(post, Left, Right, Url, Body, UserHeaders).

deleteImpl(Left, Right, Url, UserHeaders) ->
  sendImpl(delete, Left, Right, Url, <<>>, UserHeaders).

sendImpl(Method, Left, Right, Url, Body, UserHeaders) ->
  fun() ->
      ReqHeaders = [ {<<"Accept-Encoding">>, <<"gzip">>} | UserHeaders ],
      Request = spud_gun:url_to_request_record(Url,
                                               #spud_request{request_timeout = 30000
                                                            ,headers = ReqHeaders
                                                            ,method = Method
                                                            ,body = Body }),
      case spud_gun:simple_request(Request) of
        Response = {ok, StatusCode, Headers, RespBody} when StatusCode >= 200, StatusCode < 300 ->
          ?SLOG_INFO("~p -> ~p", [Request, Response]),
          %% Gun lowercases response header names
          case proplists:lookup(<<"content-encoding">>, Headers) of
            {_, <<"gzip">>} -> Right(zlib:gunzip(RespBody));
            none -> Right(RespBody)
          end;
        {ok, _Other, _Headers, Body} ->
          Right(Body);
        Response ->
          ?SLOG_DEBUG("spud gun send failed", #{url => Url,
                                                method => Method,
                                                response=> Response}),
          Left(<<"request error">>)
      end
  end.
