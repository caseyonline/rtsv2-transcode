-module(rtsv2_webrtc_session_start_resource).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").

-export([
         init/2,
         allowed_methods/2,
         service_available/2,
         resource_exists/2,
         content_types_provided/2,
         get_json/2
        ]).

-record(state, {
                exists :: boolean(),
                management_interface :: undefined | atom(),
                turn_server_public_ip :: undefined | binary_string(),
                turn_server_public_port :: integer(),
                turn_server_username :: binary_string(),
                turn_server_password :: binary_string(),
                program_details :: frame(),
                bus_name :: term(),
                use_turn :: boolean()
               }).

init(Req, MakeStreamAndVariant) ->


  ManagementInterface = lo0,
  TurnServerPublicIp = <<"127.0.0.1">>,
  TurnServerPublicPort = 4000,
  TurnServerUsername = <<"username">>,
  TurnServerPassword = <<"password">>,

  StreamId = cowboy_req:binding(stream_id, Req),
  VariantId = cowboy_req:binding(variant_id, Req),

  BusName = {webrtc_stream_output, (MakeStreamAndVariant(StreamId))(VariantId)},

  #{use_turn := UseTurn } = cowboy_req:match_qs([
                                                 { use_turn, int, 0 }
                                                ], Req),

  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-method">>, <<"GET">>, Req),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),

  {cowboy_rest, Req3, #state{
                         management_interface = ManagementInterface,
                         turn_server_public_ip = TurnServerPublicIp,
                         turn_server_public_port = TurnServerPublicPort,
                         turn_server_username = TurnServerUsername,
                         turn_server_password = TurnServerPassword,
                         bus_name = BusName,
                         use_turn = UseTurn == 1
                       }}.

resource_exists(Req, State = #state{exists = Exists}) ->
  {Exists, Req, State}.

service_available(Req, State = #state{bus_name = BusName}) ->
  case pubsub:exists(BusName) of
    true ->
      case pubsub:read_bus_metadata(BusName) of
        {ok, #live_stream_metadata{
                program_details = Frame = #frame{}
               }} ->
          {true, Req, State#state{
                        exists = true,
                        program_details = Frame
                       }};
        _ ->
          {false, Req, State#state{exists = true}}
      end;
    false ->
      {true, Req, State#state{exists = false}}
  end.

allowed_methods(Req, State) ->
  { [<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, get_json}], Req, State}.

get_json(Req, State = #state { use_turn = TurnRequested,
                               turn_server_public_ip = MaybeTurnServerPublicIp,
                               turn_server_public_port = TurnServerPublicPort,
                               turn_server_username = TurnServerUsername,
                               turn_server_password = TurnServerPassword
                             }) ->

  SessionId = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
  Host = cowboy_req:host(Req),
  TurnServerPublicIp = ?null_coalesce(MaybeTurnServerPublicIp, Host),

  { jsx:encode([
                { token, << SessionId/binary >> },
                { path, <<"session/", SessionId/binary >> },
                { turnServer, if TurnRequested  ->
                                  [
                                   { urls, [ <<"turn:", TurnServerPublicIp/binary, ":", (integer_to_binary(TurnServerPublicPort))/binary >> ] },
                                   { credential, TurnServerPassword },
                                   { username, TurnServerUsername}
                                  ];
                                 ?otherwise -> null
                              end
                }
    ]), Req, State}.
