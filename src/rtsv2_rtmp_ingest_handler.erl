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
         on_stream_callback :: fun()
        }).

init(Rtmp, ConnectArgs, [#{ init := OnConnectCallback  }]) ->

  {_, AppArg} = lists:keyfind("app", 1, ConnectArgs),

  [ShortName | Rem] = string:split(AppArg, "?"),

  Query = case Rem of
            [] -> #{};
            [QueryString] -> maps:from_list(rtmp_utils:parse_qs(list_to_binary(QueryString)))
          end,

  Response = (OnConnectCallback(list_to_binary(ShortName), Query))(),

  case Response of
    {rejectRequest} ->
      %% TODO - reject reasons? do we care?
      {stop, rejected};

    {initialResponse, #{authType := {Protocol}}} ->
      {authenticate, Protocol};

    {adobePhase1Response, Username, #{challenge := Challenge, salt := Salt}} ->
      {authenticate, adobe, Username, Salt, Challenge};

    {llnwPhase1Response, Username, #{nonce := Nonce}} ->
      {authenticate, llnw, Username, Nonce};

    {acceptRequest, OnStreamCallback} ->
      {ok, #?state{rtmp_pid = Rtmp,
                   on_stream_callback = OnStreamCallback}}
  end.

handle(#?state{rtmp_pid = Rtmp,
               on_stream_callback = OnStreamCallback}) ->

  {ok, {RemoteIp, RemotePort}} = rtmp:peername(Rtmp),
  RemoteIpStr = list_to_binary(inet:ntoa(RemoteIp)),

  receive
    {Rtmp, {request, publish, PublishArgs = {_RtmpStreamId, _ClientId, Path}}} ->

      [StreamNameStr | _] = string:tokens(Path, "?"),
      StreamName = list_to_binary(StreamNameStr),

      (OnStreamCallback(RemoteIpStr, RemotePort, StreamName, Rtmp, PublishArgs))(),

      rtmp:close(Rtmp)
  end.
