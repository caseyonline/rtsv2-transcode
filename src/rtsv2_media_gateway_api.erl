-module(rtsv2_media_gateway_api).


-behaviour(gen_server).


-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_common/include/id3as_message_bus.hrl").
-include("./rtsv2_media_gateway_api.hrl").
-include("./rtsv2_rtp.hrl").


-export([ start_link/2
        , add_egest/5
        , remove_egest/2
        , add_egest_client/4
        , remove_egest_client/1
        , update_egest_client_subscription/3
        ]).


-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).


-define(state, ?MODULE).
-define(SERVER, ?MODULE).


-define(pack(What), msgpack:pack(What, [{pack_str, none}])).
-define(unpack(What), msgpack:unpack_stream(What, [{unpack_str, as_binary}])).

-define(BLOCK_BODY_LEN_BYTES, 2).

-record(read_block,
        { remaining_len :: non_neg_integer()
        , builder = [] :: list(binary())
        }).

-record(receive_sm,
        { state = receiving_length :: receiving_length | receiving_body
        , current_block = #read_block{ remaining_len = ?BLOCK_BODY_LEN_BYTES } :: #read_block{}
        }).


-record(?state,
        { uds_path :: binary_string()
        , server_port :: undefined | port()
        , control_socket :: socket:socket()
        , receive_sm = #receive_sm{} :: #receive_sm{}
        }).


%%% ----------------------------------------------------------------------------
%%% Public API
%%% ----------------------------------------------------------------------------
start_link(ThisServerAddress, MediaGatewayFlag) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ThisServerAddress, MediaGatewayFlag], []).


add_egest(SlotId, SlotRole, ReceiveSocket, AudioSSRC, VideoSSRC) ->
  gen_server:call(?SERVER, {add_egest, SlotId, SlotRole, ReceiveSocket, AudioSSRC, VideoSSRC}).

remove_egest(SlotId, SlotRole) ->
  gen_server:call(?SERVER, {remove_egest, SlotId, SlotRole}).

add_egest_client(SlotId, SlotRole, ClientId, EgestClientConfig) ->
  gen_server:call(?SERVER, {add_egest_client, SlotId, SlotRole, ClientId, EgestClientConfig}).

remove_egest_client(ClientId) ->
  gen_server:call(?SERVER, {remove_egest_client, ClientId}).

update_egest_client_subscription(ClientId, AudioSSRC, VideoSSRC) ->
  gen_server:call(?SERVER, {update_egest_client_subscription, ClientId, AudioSSRC, VideoSSRC}).


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

handle_call({remove_egest, SlotId, SlotRole}, _From, State) ->

  NewState = ensure_control_socket(State),

  Header = header(remove_egest),

  Body = ?pack(#{ slot_key => slot_key(SlotId, SlotRole)
                }),

  send_msg(NewState#?state.control_socket, Header, Body, []),

  {reply, ok, NewState};

handle_call({ add_egest_client
            , SlotId
            , SlotRole
            , ClientId
            , #media_gateway_egest_client_config{ audio = #media_gateway_stream_element_config{ media_socket = AudioSocket
                                                                                              , egest_crypto = AudioEgestCrypto
                                                                                              , cname = AudioCName
                                                                                              , payload_type_id = AudioPayloadTypeId
                                                                                              , input_ssrc = AudioSSRC
                                                                                              }
                                                , video = #media_gateway_stream_element_config{ media_socket = VideoSocket
                                                                                              , egest_crypto = VideoEgestCrypto
                                                                                              , cname = VideoCName
                                                                                              , payload_type_id = VideoPayloadTypeId
                                                                                              , input_ssrc = VideoSSRC
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
                , audio_input_ssrc => AudioSSRC
                , video_crypto_params => convert_crypto_params(VideoEgestCrypto)
                , video_cname => VideoCName
                , video_payload_type_id => VideoPayloadTypeId
                , video_input_ssrc => VideoSSRC
                }),

  {ok, AudioSocketFd} = inet:getfd(AudioSocket),
  {ok, VideoSocketFd} = inet:getfd(VideoSocket),

  send_msg(NewState#?state.control_socket, Header, Body, [AudioSocketFd, VideoSocketFd]),

  {reply, ok, NewState};

handle_call({remove_egest_client, ClientId}, _From, State) ->

  NewState = ensure_control_socket(State),

  Header = header(remove_egest_client),

  Body = ?pack(#{ client_id => ClientId }),

  send_msg(NewState#?state.control_socket, Header, Body, []),

  {reply, ok, NewState};

handle_call({update_egest_client_subscription, ClientId, AudioSSRC, VideoSSRC}, _From, State) ->

  NewState = ensure_control_socket(State),

  Header = header(update_egest_client_subscription),

  Body = ?pack(#{ client_id => ClientId
                , audio_input_ssrc => AudioSSRC
                , video_input_ssrc => VideoSSRC
                }),

  send_msg(NewState#?state.control_socket, Header, Body, []),

  {reply, ok, NewState}.


handle_cast(not_implemented, State) ->
  {noreply, State}.


handle_info({'$socket', _Socket, select, _SelectRef}, #?state{} = State) ->
  NewState = step_receive_sm_loop(State),
  {noreply, NewState};

handle_info({'$socket', _Socket, abort, _Info}, #?state{} = State) ->
  {noreply, State};

handle_info({Port, {exit_status, Status}}, #?state{ server_port = ServerPort } = State) when ServerPort =:= Port ->
  ?SLOG_NOTICE("A port exited.", #{ port => Port, status => Status }),
  {noreply, State}.


ensure_control_socket(#?state{ control_socket = ControlSocket } = State) when ControlSocket =/= undefined ->
  State;

ensure_control_socket(State = #?state{ uds_path = UdsPath }) ->
  {ok, MediaGateway} = socket:open(local, stream),
  DBindAddress = #{ family => local, path => UdsPath },
  ok = socket:connect(MediaGateway, DBindAddress),
  NewState1 = State#?state{ control_socket = MediaGateway },
  {continue, NewState2} = step_receive_sm(NewState1),
  NewState2.


header(Type) ->
  ?pack(#{ kind => Type }).


slot_key(SlotId, SlotRole) ->
  SlotIdLow = SlotId band 16#ffffffffffffffff,
  SlotIdHigh = SlotId bsr 64,

  #{ id => [SlotIdHigh, SlotIdLow]
   , role => SlotRole
   }.


send_msg(Socket, Header, Body, Fds) ->
  MessageComponents = [ Header
                      , Body
                      ],

  WireMessage = build_frame(MessageComponents),

  MsgHdr = case Fds of
             [] ->
               #{ iov => WireMessage
                , flags => []
                };
             _ ->
               CMsgHdr = #{ level => socket
                          , type => rights
                          , data => iolist_to_binary([ <<Fd:32/little-signed-integer>> || Fd <- Fds ])
                          },
               #{ iov => WireMessage
                , ctrl => [CMsgHdr]
                , flags => []
                }
           end,
  ok = socket:sendmsg(Socket, MsgHdr),

  ok.


build_frame(Message) ->
  PackedLen = iolist_size(Message),
  [<<PackedLen:16/big-integer>>, Message].


convert_crypto_params(#srtp_crypto_params{ master_key = MasterKey, master_salt = MasterSalt }) ->
  #{ 'AESCounterMode' =>
       #{ key => MasterKey
        , salt => MasterSalt
        }
   }.


step_receive_sm_loop(State) ->
  case step_receive_sm(State) of
    { continue, NewState } ->
      NewState;

    { frame, FrameData, NewState } ->
      {Header, BodyData} = ?unpack(FrameData),
      {#{ <<"client_id">> := ServerScopedClientId } = Body, <<>>} = ?unpack(BodyData),
      EventRecord = event_to_record(Header, Body),
      ?I_RAISE_BUS_MSG(
         {media_gateway_event, ServerScopedClientId},
         #media_gateway_event{ details = EventRecord }
        ),
      step_receive_sm_loop(NewState)
  end.


step_receive_sm(#?state{ control_socket = Socket
                       , receive_sm = #receive_sm{ state = SMState, current_block = CurrentBlock } = SM
                       } = State
               ) ->

  case {read_block(Socket, CurrentBlock), SMState} of
    {{complete, <<BodyLen:16/big-integer>>}, receiving_length} ->
      step_receive_sm(State#?state{ receive_sm = SM#receive_sm{ state = receiving_body, current_block = #read_block{ remaining_len = BodyLen } } });

    {{complete, Data}, receiving_body} ->
      { frame
      , Data
      , State#?state{ receive_sm = #receive_sm{} }
      };

    {{continue, UpdatedBlock}, _SMState} ->
      { continue
      , State#?state{ receive_sm = SM#receive_sm{ current_block = UpdatedBlock } }
      }
  end.


read_block(Socket, #read_block{ remaining_len = RemainingLen, builder = Builder } = Block) ->
  case socket:recv(Socket, RemainingLen, nowait) of
    {ok, <<Data:RemainingLen/binary>>} ->
      {complete, iolist_to_binary(lists:reverse([Data | Builder]))};

    {select, _SelectInfo} ->
      {continue, Block};

    {ok, {PartialData, _SelectInfo}} ->
      NewBlock = Block#read_block{ remaining_len = RemainingLen - byte_size(PartialData), builder = [ PartialData | Builder ] },
      {continue, NewBlock}
  end.

event_to_record(#{ <<"kind">> := <<"synchronization_established">> }, #{ <<"rtp_timestamp">> := RTPTimestamp }) ->
  #media_gateway_client_synchronization_established_event{ rtp_timestamp = RTPTimestamp };

event_to_record(#{ <<"kind">> := <<"subscription_switched">> }, #{ <<"audio_ssrc">> := AudioSSRC, <<"video_ssrc">> := VideoSSRC }) ->
  #media_gateway_client_subscription_switched_event{ audio_ssrc = AudioSSRC, video_ssrc = VideoSSRC };

event_to_record(#{ <<"kind">> := <<"statistics_updated">> },
                #{ <<"audio_packets_sent">> := AudioPacketsSent
                 , <<"audio_octets_sent">> := AudioOctetsSent
                 , <<"video_packets_sent">> := VideoPacketsSent
                 , <<"video_octets_sent">> := VideoOctetsSent
                 }) ->
  #media_gateway_client_statistics_updated_event{ audio_packets_sent = AudioPacketsSent
                                                , audio_octets_sent = AudioOctetsSent
                                                , video_packets_sent = VideoPacketsSent
                                                , video_octets_sent = VideoOctetsSent
                                                }.
