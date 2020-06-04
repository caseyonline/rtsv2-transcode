-module(rtsv2_internal_playlist_publish).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_common/include/spud_gun.hrl").
-include_lib("kernel/include/inet.hrl").

% -include("../../include/ts_akamai_writer.hrl").

-export([
         send/5
        ]).

-record(send_state, {
          base_url :: binary_string(),
          auth :: {binary_string(), binary_string()},
          playlists :: list({binary_string(), binary_string()}),
          completion_msg :: term(),
          parent :: pid(),
          cached_ip :: undefined | inet:ip_address(),
          cached_hostname :: undefined | binary_string(),
          spud_pid :: undefined | pid(),
          version_string :: binary_string()
         }).

send(Auth, BaseUrl, Playlists, CompletionMsg, VersionString) ->
  Self = self(),
  BaseUrl1 = case binary:last(BaseUrl) == $/ of
    true -> BaseUrl;
    false -> <<BaseUrl/binary, "/">>
  end,
  spawn_link(fun() ->
                 monitor(process, Self),
                 send_playlists(#send_state {
                                        base_url = BaseUrl1,
                                        auth = Auth,
                                        playlists = Playlists,
                                        completion_msg = CompletionMsg,
                                        version_string = VersionString,
                                        parent = Self
                                       })
             end).

send_playlists(_State = #send_state { playlists = [], parent = Parent, completion_msg = Msg}) ->
  Parent ! Msg,
  ok;

send_playlists(State = #send_state {
                          base_url = BaseUrl,
                          cached_ip = undefined
                         }) ->
  { ok, {_Scheme, _UserInfo, Host, _Port, _Path, _Query} } = http_uri:parse(binary_to_list(BaseUrl)),
  case inet:gethostbyname(Host) of
    { ok, #hostent { h_addr_list = [ Head | _ ] } } ->
      send_playlists(State#send_state { cached_ip = Head, cached_hostname = list_to_binary(Host) });
    _ ->
      start_retry(State)
  end;

send_playlists(State = #send_state {
                          base_url = BaseUrl,
                          auth = {Username, Password},
                          cached_ip = HostIp,
                          cached_hostname = Host,
                          playlists = [ { PlaylistName, PlaylistData } | Tail ],
                          spud_pid = SpudPid,
                          version_string = VersionString
                         }) ->

  Url = << BaseUrl/binary, PlaylistName/binary >>,
  BasicAuth = base64:encode(<<Username/binary,":",Password/binary>>),
  SimpleRequest = spud_gun:url_to_request_record(Url, #spud_request {
                                                         method = put,
                                                         body = PlaylistData,
                                                         headers = [ { <<"host">>, Host },
                                                                    %  { <<"user-agent">>, ?AKAMAI_USER_AGENT(VersionString) },
                                                                     { <<"connection">>, <<"keep-alive">> },
                                                                     { <<"content-type">>, <<"application/octet-stream">>},
                                                                     { <<"authorization">>, <<"Basic ", BasicAuth/binary>>}
                                                                   ],
                                                         spud_pid = SpudPid
                                                        }),
  SimpleRequestWithCachedIp = SimpleRequest#spud_request { remote_host = i_convert:convert(HostIp, binary_string), return_spud_response = true },
  case spud_gun:simple_request(SimpleRequestWithCachedIp) of
    { ok, #spud_response {
             http_code = Status,
             spud_pid = NewSpudPid,
             headers = _OutputHeaders,
             body = Body
            }} ->
      if Status >= 200 andalso Status =< 299 ->
          %%?INFO("Posted ~p to ~p successfully", [ PlaylistName, BaseUrl ]),
           send_playlists(State#send_state {
                            spud_pid = NewSpudPid,
                            playlists = Tail
                           });
         Status == 400 orelse Status == 403 ->
           ?WARNING("Failed to post playlist ~p with error ~p:~p", [ PlaylistName, Status, Body ]),
           start_retry(State);
         ?otherwise ->
           ?WARNING("Failed to post playlist ~p with http status ~p", [ PlaylistName, Status ]),
           start_retry(State)
      end;
    { error, Reason } ->
      ?WARNING("Failed to post playlist ~p with reason ~p", [ PlaylistName, Reason ]),
      start_retry(State)
  end.

start_retry(State = #send_state {}) ->
  receive
    _ -> ok %% Can only be an exit really
  after 1000 ->
          send_playlists(State#send_state {
                           cached_ip = undefined,
                           cached_hostname = undefined,
                           spud_pid = undefined
                          })
  end.
