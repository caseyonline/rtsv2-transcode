-module(rtsv2_media_gateway_api).


-behaviour(gen_server).


-include_lib("id3as_common/include/common.hrl").
-include("./rtsv2_media_gateway_api.hrl").
-include("./rtsv2_rtp.hrl").


-export([ start_link/2
        , add_egest/5
        , add_egest_client/4
        ]).


-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).


-define(state, ?MODULE).
-define(SERVER, ?MODULE).


-define(pack(What),                      msgpack:pack(What, [{pack_str, none}])).


-record(?state,
        { uds_path :: binary_string()
        , server_port :: undefined | port()
        , control_socket :: socket:socket()
        }).


%%% ----------------------------------------------------------------------------
%%% Public API
%%% ----------------------------------------------------------------------------
start_link(ThisServerAddress, MediaGatewayFlag) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ThisServerAddress, MediaGatewayFlag], []).


add_egest(SlotId, SlotRole, ReceiveSocket, AudioSSRC, VideoSSRC) ->
  gen_server:call(?SERVER, {add_egest, SlotId, SlotRole, ReceiveSocket, AudioSSRC, VideoSSRC}).


add_egest_client(SlotId, SlotRole, ClientId, EgestClientConfig) ->
  gen_server:call(?SERVER, {add_egest_client, SlotId, SlotRole, ClientId, EgestClientConfig}).


%%% ----------------------------------------------------------------------------
%%% Gen Server Implementation
%%% ----------------------------------------------------------------------------
init([ThisServerAddress, MediaGatewayFlag]) ->
  process_flag(trap_exit, true),

  State =
    case MediaGatewayFlag of
      {on} ->
        Cmd = filename:join([code:priv_dir(rtsv2), "scripts", "runMediaGateway.sh"]),
        UdsPath = <<"/tmp/rtsv2-media-gateway-", ThisServerAddress/binary, ".sock">>,
        Port = erlang:open_port({spawn_executable, Cmd}, [exit_status, {args, [UdsPath]}]),

        #?state{ uds_path = UdsPath
               , server_port = Port
               };

      {external} ->
        #?state{ uds_path = <<"/tmp/rtsv2-media-gateway.sock">>
               , server_port = undefined
               };

      {both} ->
        #?state{ uds_path = <<"/tmp/rtsv2-media-gateway.sock">>
               , server_port = undefined
               }
    end,

  {ok, State}.


handle_call({add_egest, SlotId, SlotRole, ReceiveSocket, AudioSSRC, VideoSSRC}, _From, State) ->

  NewState = ensure_control_socket(State),

  Header = header(add_egest),

  Body = ?pack(#{ slot_key => slot_key(SlotId, SlotRole)
                , audio_payload_type_id => ?OPUS_ENCODING_ID
                , audio_output_ssrc => AudioSSRC
                , video_payload_type_id => ?H264_ENCODING_ID
                , video_output_ssrc => VideoSSRC
                }),

  {ok, ReceiveFD} = inet:getfd(ReceiveSocket),

  send_msg(NewState#?state.control_socket, Header, Body, [ReceiveFD]),

  {reply, ok, NewState};

handle_call({ add_egest_client
            , SlotId
            , SlotRole
            , ClientId
            , #media_gateway_egest_client_config{ audio = #media_gateway_stream_element_config{ media_socket = AudioSocket
                                                                                              , egest_crypto = AudioEgestCrypto
                                                                                              , cname = AudioCName
                                                                                              , payload_type_id = AudioPayloadTypeId
                                                                                              }
                                                , video = #media_gateway_stream_element_config{ media_socket = VideoSocket
                                                                                              , egest_crypto = VideoEgestCrypto
                                                                                              , cname = VideoCName
                                                                                              , payload_type_id = VideoPayloadTypeId
                                                                                              }
                                                }
            }
           , _From
           , State
           ) ->

  NewState = ensure_control_socket(State),

  Header = header(add_egest_client),

  Body = ?pack(#{ slot_key => slot_key(SlotId, SlotRole)
                , client_id => ClientId
                , audio_crypto_params => convert_crypto_params(AudioEgestCrypto)
                , audio_cname => AudioCName
                , audio_payload_type_id => AudioPayloadTypeId
                , video_crypto_params => convert_crypto_params(VideoEgestCrypto)
                , video_cname => VideoCName
                , video_payload_type_id => VideoPayloadTypeId
                }),

  {ok, AudioSocketFd} = inet:getfd(AudioSocket),
  {ok, VideoSocketFd} = inet:getfd(VideoSocket),

  send_msg(NewState#?state.control_socket, Header, Body, [AudioSocketFd, VideoSocketFd]),

  {reply, ok, NewState}.

handle_cast(not_implemented, State) ->
  {noreply, State}.


handle_info({Port, {exit_status, Status}}, #?state{ server_port = ServerPort } = State) when ServerPort =:= Port ->
  ?SLOG_NOTICE("A port exited.", #{ port => Port, status => Status }),
  {noreply, State}.


ensure_control_socket(#?state{ control_socket = ControlSocket } = State) when ControlSocket =/= undefined ->
  State;

ensure_control_socket(State = #?state{ uds_path = UdsPath }) ->
  {ok, MediaGateway} = socket:open(local, stream),
  DBindAddress = #{ family => local, path => UdsPath },
  ok = socket:connect(MediaGateway, DBindAddress),
  State#?state{ control_socket = MediaGateway }.


header(Type) ->
  ?pack(#{ kind => Type }).


slot_key(SlotId, SlotRole) ->
  SlotIdLow = SlotId band 16#ffffffffffffffff,
  SlotIdHigh = SlotId bsr 64,

  #{ id => [SlotIdHigh, SlotIdLow]
   , role => SlotRole
   }.


send_msg(Socket, Header, Body, Fds) ->
  io:format(user, "Header: ~p~n", [Header]),
  io:format(user, "Body: ~p~n", [Body]),

  MessageComponents = [ Header
                      , Body
                      ],

  WireMessage = build_frame(MessageComponents),

  CmsgHdr = #{ level => socket
             , type => rights
             , data => iolist_to_binary([ <<Fd:32/little-signed-integer>> || Fd <- Fds ])
             },

  MsgHdr = #{ iov => WireMessage
            , ctrl => [CmsgHdr]
            , flags => []
            },

  io:format("Sending on socket ~p", [Socket]),

  ok = socket:sendmsg(Socket, MsgHdr),

  io:format("Sent on socket ~p", [Socket]).


build_frame(Message) ->
  PackedLen = iolist_size(Message),
  [<<PackedLen:16/big-integer>>, Message].


convert_crypto_params(#srtp_crypto_params{ master_key = MasterKey, master_salt = MasterSalt }) ->
  #{ 'AESCounterMode' =>
       #{ key => MasterKey
        , salt => MasterSalt
        }
   }.
