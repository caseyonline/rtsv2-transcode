-module(spudGun@foreign).

-export([
         makeRequestImpl/6
        ]).

-include_lib("id3as_common/include/spud_gun.hrl").
-include_lib("id3as_common/include/common.hrl").

%% makeHeadersImpl(Record) ->
%%   Headers = lists:map(fun({K, V}) -> {atom_to_binary(K, utf8), V} end,
%%                       maps:to_list(Record)),
%%   io:format(user, "headers ~p~n", [Headers]),
%%   Headers.

makeRequestImpl(ReqError, RespError, RespSuccess, Method, Url, Options) ->
  fun() ->
      #spud_request{ connect_timeout = DefaultConnectTimeout
                   , request_timeout = DefaultRequestTimeout
                   , body_timeout    = DefaultBodyTimeout
                   } = #spud_request{},

      UserHeaders = maps:get(headers, Options, []),
      Body = maps:get(body, Options, <<>>),
      ConnectTimeout = maps:get(request_timeout, Options, DefaultConnectTimeout),
      RequestTimeout = maps:get(request_timeout, Options, DefaultRequestTimeout),
      BodyTimeout = maps:get(request_timeout, Options, DefaultBodyTimeout),
      FollowRedirect = maps:get(followRedirect, Options, false),

      ReqHeaders = [ {<<"accept-encoding">>, <<"gzip">>} | UserHeaders ],

      Request = spud_gun:url_to_request_record(Url,
                                               #spud_request{ connect_timeout = ConnectTimeout
                                                            , request_timeout = RequestTimeout
                                                            , body_timeout    = BodyTimeout
                                                            , headers = ReqHeaders
                                                            , method = Method
                                                            , body = Body
                                                            , options = #{follow_redirect => FollowRedirect}
                                                            }),
      case spud_gun:simple_request(Request) of
        {ok, StatusCode, Headers, RespBody} when StatusCode >= 200, StatusCode < 300 ->
          %% Gun lowercases response header names
          UnzippedBody = case proplists:lookup(<<"content-encoding">>, Headers) of
                           {_, <<"gzip">>} -> zlib:gunzip(RespBody);
                           none -> RespBody
                         end,
          ((RespSuccess(StatusCode))(Headers))(UnzippedBody);

        {ok, StatusCode, Headers, RespBody} ->
          ((RespError(StatusCode))(Headers))(RespBody);

        Error ->
          ReqError(Error)
      end
  end.
