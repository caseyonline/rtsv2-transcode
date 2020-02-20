-module(rtsv2_rtmp_ingest_handler).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/rtmp.hrl").

-export([
         init/3,
         handle/1
        ]).

-define(state, ?MODULE).

-record(?state,
        {
         rtmp_pid :: pid(),
         handle :: fun()
        }).

init(Rtmp, ConnectArgs, [#{ init := Init  }]) ->

  {_, TcUrl} = lists:keyfind("tcUrl", 1, ConnectArgs),

  #rtmp_parsed_url{
     connection_config = #rtmp_connection_config{
                            host = Host
                           },
     app_config = #rtmp_app_config{
                     application = ShortName
                    },
     query = Query
    } = rtmp_utils:parse_url(TcUrl),

  Response = (Init(Host, ShortName, Query))(),

  case Response of
    {rejectRequest} ->
      %% TODO - reject reasons? do we care?
      {stop, rejected};

    {initialResponse, #{authType := {Protocol}}} ->
      {authenticate, Protocol};

    {adobePhase1Response, Username, #{challenge := Challenge, salt := Salt}} ->
      {authenticate, adobe, Username, Salt, Challenge};

    {acceptRequest, Handle} ->
      {ok, #?state{rtmp_pid = Rtmp,
                   handle = Handle}}
  end.

  %% State = #?state{rtmp_pid = Rtmp,
  %%                 ingestStartedFn = IngestStartedFn,
  %%                 ingestStoppedFn = IngestStoppedFn,
  %%                 streamAuthTypeFn = StreamAuthTypeFn,
  %%                 streamAuthFn = StreamAuthFn,
  %%                 clientMetadataFn = ClientMetadataFn,
  %%                 sourceInfoFn = SourceInfoFn
  %%                },

  %% case Query of
  %%   #{<<"authmod">> := <<"adobe">>,
  %%     <<"user">> := UserName,
  %%     <<"challenge">> := ClientChallenge,
  %%     <<"response">> := ClientResponse} ->
  %%     %% We have a adobe completed digest

  %%     Reply = (StreamAuthFn(Host, ShortName, UserName))(),

  %%     case Reply of
  %%       {nothing} ->
  %%         ?SLOG_INFO("Abobe StreamAuth rejected", #{host => Host,
  %%                                                   shortName => ShortName,
  %%                                                   username => UserName}),
  %%         {stop, rejected};

  %%       {just, #{username := ExpectedUserName,
  %%                password := ExpectedPassword}} ->

  %%         case rtmp:compare_adobe_challenge_response(ExpectedUserName,
  %%                                                    <<"NzEwNzk">>,
  %%                                                    ExpectedPassword,
  %%                                                    <<"ODE3MDQ3NTYz">>,
  %%                                                    ClientChallenge,
  %%                                                    ClientResponse) of
  %%           true ->
  %%             {ok, State#?state{streamPublishFn = StreamPublishFn(Host, ShortName, UserName)}};

  %%           false ->
  %%             ?SLOG_INFO("Challenge failed", #{client_challenge => ClientChallenge,
  %%                                              client_response => ClientResponse}),
  %%             {authenticate_fail, adobe, invalid_password}
  %%         end
  %%     end;

  %%   #{<<"authmod">> := <<"adobe">>,
  %%     <<"user">> := UserName} ->
  %%     ?INFO("Username Matches - do next phase"),
  %%     {authenticate, adobe, UserName, <<"NzEwNzk">>, <<"ODE3MDQ3NTYz">>};

  %%   #{<<"authmod">> := <<"llnw">>,
  %%     <<"user">> := UserName,
  %%     <<"nonce">> := ClientOurNonce,
  %%     <<"cnonce">> := ClientNonce,
  %%     <<"nc">> := ClientNc,
  %%     <<"response">> := ClientResponse} ->
  %%     %% We have a llnw completed digest

  %%     Reply = (StreamAuthFn(Host, ShortName, UserName))(),

  %%     case Reply of
  %%       {nothing} ->
  %%         ?SLOG_INFO("LLNW StreamAuth rejected", #{host => Host,
  %%                                                  shortName => ShortName,
  %%                                                  username => UserName}),
  %%         {stop, rejected};

  %%       {just, #{username := ExpectedUserName,
  %%                password := ExpectedPassword}} ->
  %%         Realm = <<"live">>,
  %%         Method = <<"publish">>,
  %%         Nonce = <<"ODE3MDQ3NTYz">>,
  %%         Qop = <<"auth">>,

  %%         case rtmp:compare_llnw_challenge_response(ExpectedUserName,
  %%                                                   Realm,
  %%                                                   ExpectedPassword,
  %%                                                   Method,
  %%                                                   ShortName,
  %%                                                   Nonce,
  %%                                                   ClientNc,
  %%                                                   ClientNonce,
  %%                                                   Qop,
  %%                                                   ClientResponse) of
  %%           true ->
  %%             {ok, State#?state{streamPublishFn = StreamPublishFn(Host, ShortName, UserName)}};

  %%           false ->
  %%             ?SLOG_INFO("Challenge failed", #{client_response => ClientResponse}),
  %%             {authenticate_fail, adobe, invalid_password}
  %%         end
  %%     end;


  %%   #{<<"authmod">> := <<"llnw">>,
  %%     <<"user">> := UserName} ->
  %%     ?INFO("Username Matches - do next phase"),
  %%     {authenticate, llnw, UserName, <<"ODE3MDQ3NTYz">>};

  %%   #{} ->
  %%     ?SLOG_INFO("No authmod - start authentication", #{host => Host,
  %%                                                       shortName => ShortName}),
  %%     Reply = (StreamAuthTypeFn(Host, ShortName))(),

  %%     case Reply of
  %%       {nothing} ->
  %%         {stop, rejected};

  %%       {just, #{authType := {Protocol}}} ->
  %%         {authenticate, Protocol}
  %%     end
  %% end.

handle(#?state{rtmp_pid = Rtmp,
               handle = Handle}) ->

  {ok, {RemoteIp, RemotePort}} = rtmp:peername(Rtmp),
  RemoteIpStr = list_to_binary(inet:ntoa(RemoteIp)),

  receive
    {Rtmp, {request, publish, PublishArgs = {_StreamId, _ClientId, Path}}} ->

      [StreamNameStr | _] = string:tokens(Path, "&"),
      StreamName = list_to_binary(StreamNameStr),

      (Handle(RemoteIpStr, RemotePort, StreamName, Rtmp, PublishArgs))(),

      rtmp:close(Rtmp)
  end.
