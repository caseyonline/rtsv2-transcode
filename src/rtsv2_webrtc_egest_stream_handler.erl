-module(rtsv2_webrtc_egest_stream_handler).


-behavior(webrtc_stream_handler).


-export([ port_number/1
        ]).


-export([ init/1
        , handle_info/2
        , handle_call/3
        ]).


-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").
-include("./src/rtsv2_slot_media_source_publish_processor.hrl").
-include("./src/rtsv2_rtp.hrl").


-define(state, ?MODULE).

-record(?state,
        { parent_pid :: pid()
        , egest_key :: term()
        , receive_socket :: gen_udp:socket()
        , parse_info = rtsv2_rtp_util:build_parse_info()
        }).


port_number(EgestKey) ->
  webrtc_stream_server:call_stream_handler(EgestKey, port_number).


init(_Args = [ ParentPid, {egestKey, << SlotId:128/big-unsigned-integer >>, {SlotRole}} = EgestKey ]) ->

  process_flag(trap_exit, true),

  %% NOTE: Erlang doesn't receive on this socket, that task is delegated to the
  %% media gateway
  {ok, ReceiveSocket} = gen_udp:open(0, [{recbuf, 100 * 1500}, {active, false}]),

  ok = rtsv2_media_gateway_api:add_egest(SlotId, SlotRole, ReceiveSocket),

  #?state{ parent_pid = ParentPid
         , egest_key = EgestKey
         , receive_socket = ReceiveSocket
         }.


handle_call(port_number, _From, #?state{ receive_socket = ReceiveSocket } = State) ->
  { ok, ReceivePort } = inet:port(ReceiveSocket),
  { reply, {ok, ReceivePort}, State }.

handle_info({'EXIT', MaybeParentPid, _Reason }, #?state{ parent_pid = ParentPid } = State) when MaybeParentPid =:= ParentPid ->
  { stop, State }.
