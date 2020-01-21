-module(rtsv2_webrtc_session_resource).

-include_lib("kernel/include/inet.hrl").
-include_lib("id3as_rtc/include/webrtc.hrl").
-include_lib("id3as_common/include/common.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state,
        {
         stream_server_id :: term()
        , session_id :: binary()
        , local_address :: webrtc_session_manager:host()
        }).

init(Req, MakeStreamAndVariant) ->

  StreamId = cowboy_req:binding(stream_id, Req),
  VariantId = cowboy_req:binding(variant_id, Req),
  SessionId = cowboy_req:binding(session_id, Req),

  StreamAndVariant = (MakeStreamAndVariant(StreamId))(VariantId),

  Host = cowboy_req:host(Req),
  {ok, #hostent{h_addr_list = AddrList}} = inet:gethostbyname(binary_to_list(Host)),
  Ip = i_convert:convert(hd(AddrList), binary_string),

  ?INFO("Starting websocket for webrtc session ~p (Host ~p, Ip ~p)", [ SessionId, Host, Ip ]),

  {cowboy_websocket
  , Req
  , #state{stream_server_id = StreamAndVariant,
           session_id = SessionId,
           local_address = Ip
          }
  , #{idle_timeout => infinity %% TODO 30000
     , max_frame_size => 128000 %% TODO: find a good value for this
     }
  }.

websocket_init(State = #state{session_id = SessionId}) ->

  webrtc_stream_server:subscribe_for_msgs(SessionId, [?ensure_record(webrtc_session_response)]),

  {reply, {text, jsx:encode([{type, <<"join">>}])}, State}.

websocket_handle({text, JSON}, State) ->
  case jsx:decode(JSON, [return_maps]) of
    Map = #{ <<"type">> := Type } ->
      MaybeMessageData = maps:get(<<"data">>, Map, undefined),

      case handle_message(Type, MaybeMessageData, State) of
        {ok, NewState} ->
          { ok
          , NewState
          , hibernate %% very little traffic goes over the signalling channel, so hibernate when we're done
          };
        {reply, Reply, NewState} ->
          { reply
          , {text, jsx:encode(Reply)}
          , NewState
          , hibernate %% very little traffic goes over the signalling channel, so hibernate when we're done
          }
      end
  end.

websocket_info(#webrtc_session_response{ payload = Payload }, Data) ->
  { reply
  , {text, jsx:encode(make_session_response(Payload))}
  , Data
  , hibernate %% very little traffic goes over the signalling channel, so hibernate when we're done
  };

websocket_info(_Info, Data) ->
  ?INFO("OTHER ~p~n", [_Info]),
  {ok, Data}.

terminate(Reason, _PartialReq, #state{ stream_server_id = ServerId,
                                       session_id = SessionId }) ->
  ?DEBUG("WebSocket for session ~p closing with reason ~p.", [SessionId, Reason]),

  webrtc_stream_server:cast_session(ServerId, SessionId, notify_socket_disconnect),

  ok.

handle_message(<<"rtc">>,
               Payload,
               State = #state{ stream_server_id = ServerId
                             , session_id = SessionId
                             , local_address = LocalAddress
                             }
              ) ->

  StartOptions =
    #{ session_id => SessionId
     , local_address => LocalAddress
     , handler_module => rtsv2_webrtc_session_handler
     , handler_args => SessionId
     , rtp_egest_config =>
         { passthrough
         , #{ audio_ssrc => 10
            , video_ssrc => 20
            }
         }
     },

  case webrtc_stream_server:handle_request(ServerId, SessionId, StartOptions, Payload) of
    {ok, #webrtc_session_response{payload = ResponsePayload}} ->
      {reply, make_session_response(ResponsePayload), State};
    ok ->
      {ok, State};
    {error, unsupported} ->
      {ok, State}
  end;

handle_message(<<"ping">>, MessageData, State) ->
  {reply, make_response(<<"pong">>, MessageData), State}.

make_session_response(Payload) ->
  make_response(rtc, Payload).

make_response(Type, undefined) ->
  [ {type, Type}
  ];

make_response(Type, Payload) ->
  [ {type, Type}
  , {data, Payload}
  ].
