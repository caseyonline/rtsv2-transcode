-module(spudGun@foreign).

-export([
         getImpl/5,
         putImpl/6,
         postImpl/6,
         deleteImpl/5
        ]).

-include_lib("id3as_common/include/spud_gun.hrl").
-include_lib("id3as_common/include/common.hrl").

getImpl(ReqError, RespError, RespSuccess, Url, ReqHeaders) ->
  fun() ->
      case spud_gun:get(Url, ReqHeaders) of
        {ok, StatusCode, Headers, RespBody} when StatusCode >= 200, StatusCode < 300 ->
          ((RespSuccess(StatusCode))(Headers))(RespBody);

        {ok, StatusCode, Headers, RespBody} ->
          ((RespError(StatusCode))(Headers))(RespBody);

        Error ->
          ?SLOG_DEBUG("spud gun get failed", #{url => Url,
                                               error => Error}),
          ReqError(Error)
      end
  end.

putImpl(ReqError, RespError, RespSuccess, Url, Body, UserHeaders) ->
  sendImpl(put, ReqError, RespError, RespSuccess, Url, Body, UserHeaders).

postImpl(ReqError, RespError, RespSuccess, Url, Body, UserHeaders) ->
  sendImpl(post, ReqError, RespError, RespSuccess, Url, Body, UserHeaders).

deleteImpl(ReqError, RespError, RespSuccess, Url, UserHeaders) ->
  sendImpl(delete, ReqError, RespError, RespSuccess, Url, <<>>, UserHeaders).

sendImpl(Method, ReqError, RespError, RespSuccess, Url, Body, UserHeaders) ->
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
          UnzippedBody = case proplists:lookup(<<"content-encoding">>, Headers) of
                           {_, <<"gzip">>} -> zlib:gunzip(RespBody);
                           none -> RespBody
                         end,
          ((RespSuccess(StatusCode))(Headers))(UnzippedBody);

        {ok, StatusCode, Headers, RespBody} ->
          ((RespError(StatusCode))(Headers))(RespBody);

        Error ->
          ?SLOG_DEBUG("spud gun send failed", #{url => Url,
                                                method => Method,
                                                error => Error}),
          ReqError(Error)
      end
  end.
