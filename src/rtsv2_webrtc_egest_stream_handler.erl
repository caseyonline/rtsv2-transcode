-module(rtsv2_webrtc_egest_stream_handler).


-behavior(webrtc_stream_handler).


-export([ port_number/1
        , set_slot_configuration/2
        ]).


-export([ init/1
        , handle_info/2
        , handle_call/3
        , terminate/2
        ]).


-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").
-include("./src/rtsv2_slot_media_source_publish_processor.hrl").
-include("./src/rtsv2_rtp.hrl").


-define(state, ?MODULE).

-record(?state,
        { slot_id :: non_neg_integer()
        , slot_role :: primary | backup
        , parent_pid :: pid()
        , egest_key :: term()
        , receive_socket :: gen_udp:socket()
        , parse_info = rtsv2_rtp_util:build_parse_info()
        , media_gateway :: not_in_use | pending_initialization | active
        }).


port_number(EgestKey) ->
  webrtc_stream_server:call_stream_handler(EgestKey, port_number).

set_slot_configuration(EgestKey, SlotConfiguration) ->
  webrtc_stream_server:call_stream_handler(EgestKey, {set_slot_configuration, SlotConfiguration}).

init([ ParentPid
     , {egestKey, << SlotId:128/big-unsigned-integer >>, {SlotRole}} = EgestKey
     , MediaGatewayFlag
     ]) ->

  process_flag(trap_exit, true),

  ?INFO("Egest Stream Handler starting with MediaGateway = ~p", [MediaGatewayFlag]),

  %% NOTE: Erlang doesn't receive on this socket, that task is delegated to the
  %% media gateway
  {TheReceiveSocket, MediaGatewayState} =
    case MediaGatewayFlag of
      {off} ->
        {ok, ReceiveSocket} = gen_udp:open(0, [binary, {recbuf, 100 * 1500}]),
        {ReceiveSocket, not_in_use};

      _OnOrExternal ->
        {ok, ReceiveSocket} = gen_udp:open(0, [{recbuf, 100 * 1500}, {active, false}]),
        {ReceiveSocket, pending_initialization}
    end,

  #?state{ slot_id = SlotId
         , slot_role = SlotRole
         , parent_pid = ParentPid
         , egest_key = EgestKey
         , receive_socket = TheReceiveSocket
         , media_gateway = MediaGatewayState
         }.

handle_call(port_number, _From, #?state{ receive_socket = ReceiveSocket } = State) ->
  { ok, ReceivePort } = inet:port(ReceiveSocket),
  { reply, {ok, ReceivePort}, State };

handle_call({set_slot_configuration, _SlotConfiguration}, _From, #?state{ media_gateway = not_in_use } = State) ->
  { reply, ok, State };

handle_call({set_slot_configuration, _SlotConfiguration}, _From, #?state{ media_gateway = active } = State) ->
  { reply, ok, State };

handle_call({set_slot_configuration, #{ profiles := Profiles, audioOnly := AudioOnly } = _SlotConfiguration},
            _From,
            #?state{ media_gateway = pending_initialization
                   , slot_id = SlotId
                   , slot_role = SlotRole
                   , receive_socket = ReceiveSocket
                   } = State) ->

  ?INFO("Egest Stream Handler received slot configuration for the first time", []),
        { AudioSSRC, VideoSSRC } =
          case SlotRole of
            primary ->
              { ?make_audio_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 0)
              , ?make_video_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 0)
              };
            backup ->
              { ?make_audio_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 1)
              , ?make_video_ssrc(?PROFILE_INDEX_RESERVED_EGEST, 1)
              }
          end,
  MaybeVideoSSRC = case AudioOnly of
                      true -> undefined;
                      false -> VideoSSRC
                   end,

  MaxBitrate = lists:max([Bitrate || #{ bitrate := Bitrate } <- Profiles]),

  ok = rtsv2_media_gateway_api:add_egest(SlotId, SlotRole, ReceiveSocket, MaxBitrate, AudioSSRC, MaybeVideoSSRC),

  { reply, ok, State#?state{ media_gateway = active } }.

handle_info({udp, ActualSocket, _SenderIP, _SenderPort, Data},
             #?state{ parse_info = ParseInfo
                    , receive_socket = ExpectedSocket
                    } = State
            )
   when
     ActualSocket =:= ExpectedSocket ->

   RTP = #rtp{ payload_type = #rtp_payload_type{ encoding_id = EncodingId } } = rtp:parse(avp, Data, ParseInfo),

   Sequence =
     case EncodingId of
       ?OPUS_ENCODING_ID ->
         #rtp_sequence{ type = audio
                      , codec = opus
                      , rtps = [ RTP ]
                      };
       ?H264_ENCODING_ID ->
         #rtp_sequence{ type = video
                      , codec = h264
                      , rtps = [ RTP ]
                      }
     end,

   { broadcast, Sequence, State };

handle_info({'EXIT', MaybeParentPid, _Reason }, #?state{ parent_pid = ParentPid } = State) when MaybeParentPid =:= ParentPid ->
  { stop, State }.


terminate(Reason, #?state{ slot_id = SlotId, slot_role = SlotRole, media_gateway = MediaGatewayState }) ->
  ?INFO("Egest Stream Handler stopping with reason ~p.", [Reason]),

  case MediaGatewayState of
    active ->
      ok = rtsv2_media_gateway_api:remove_egest(SlotId, SlotRole),
      ok;
    _ ->
      ok
  end,

  ok.
