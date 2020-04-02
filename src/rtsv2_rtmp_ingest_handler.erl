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

    {llnwPhase1Response, Username, #{nonce := Nonce}} ->
      {authenticate, llnw, Username, Nonce};

    {acceptRequest, Handle} ->
      {ok, #?state{rtmp_pid = Rtmp,
                   handle = Handle}}
  end.

handle(#?state{rtmp_pid = Rtmp,
               handle = Handle}) ->

  {ok, {RemoteIp, RemotePort}} = rtmp:peername(Rtmp),
  RemoteIpStr = list_to_binary(inet:ntoa(RemoteIp)),

  receive
    {Rtmp, {request, publish, PublishArgs = {_RtmpStreamId, _ClientId, Path}}} ->

      [StreamNameStr | _] = string:tokens(Path, "?"),
      StreamName = list_to_binary(StreamNameStr),

      (Handle(RemoteIpStr, RemotePort, StreamName, Rtmp, PublishArgs))(),

      rtmp:close(Rtmp)
  end.
