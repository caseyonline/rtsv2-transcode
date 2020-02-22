-module(rtsv2_agents_egestInstance@foreign).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").
-include("../../../../src/rtsv2_slot_media_source_publish_processor.hrl").
-include("../../../../src/rtsv2_rtp.hrl").


%% foreign exports
-export([ startEgestReceiverFFI/1
        , setSlotConfigurationFFI/2
        , getSlotConfigurationFFI/1
        ]).


%% proc_lib exports
-export([ init/2
        ]).


-define(state, ?MODULE).


-record(?state,
        { receive_socket :: gen_udp:socket()
        , egest_key :: term()
        , parse_info = rtsv2_rtp_util:build_parse_info()
        , stream_server_pid :: undefined | pid()
        }).


-define(metadata, rtsv2_agents_streamRelayInstance_metadata).

-record(?metadata,
        { slot_configuration :: rtsv2_slot_configuration:slot_configuration()
        }).


startEgestReceiverFFI(EgestKey) ->
  fun() ->
      {ok, _ChildProc, PortNumber} = proc_lib:start_link(?MODULE, init, [self(), EgestKey]),
      PortNumber
  end.


setSlotConfigurationFFI(EgestKey, SlotConfiguration) ->
  fun() ->
      _ = gproc:add_local_property({metadata, EgestKey},
                                   #?metadata{ slot_configuration = SlotConfiguration }
                                  )
  end.


getSlotConfigurationFFI(EgestKey) ->
  fun() ->
      case gproc:lookup_local_properties({metadata, EgestKey}) of
        [] ->
          {nothing};

        [{_Pid, #?metadata{ slot_configuration = SlotConfiguration }}] ->
          {just, SlotConfiguration}
      end
  end.


init(Parent, EgestKey) ->
  process_flag(trap_exit, true),
  case init_core(EgestKey) of
    {ok, #?state{ receive_socket = ReceiveSocket } = State} ->

      {ok, PortNumber} = inet:port(ReceiveSocket),

      {ok, StreamServerPid} =
        webrtc_stream_server:start_link(EgestKey,
                                        #{ stream_module => rtsv2_webrtc_stream_handler
                                         , stream_module_args => [ EgestKey ]
                                         }
                                       ),
      Ret =
        { ok
        , self()
        , PortNumber
        },

      proc_lib:init_ack(Parent, Ret),

      loop(Parent,
           sys:debug_options([]),
           State#?state{stream_server_pid = StreamServerPid}
          );

    {error, Reason} ->
      exit(Reason)
  end.


init_core(EgestKey) ->
  case gen_udp:open(0, [binary, {recbuf, 50 * 1500}]) of
    {ok, ReceiveSocket} ->
      { ok
      , #?state{ receive_socket = ReceiveSocket
               , egest_key = EgestKey
               }
      };

    Err ->
      Err
  end.


loop(Parent, Debug, #?state{ parse_info = ParseInfo, egest_key = EgestKey, stream_server_pid = StreamServerPid } = State) ->
  receive
    {udp, _ReceiveSocket, _SenderIP, _SenderPort, Data} ->

      RTP = #rtp{ payload_type = #rtp_payload_type{ encoding_id = EncodingId } } = rtp:parse(avp, Data, ParseInfo),

      pubsub:publish({webrtc_stream_output, EgestKey},
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
                     end
                    ),

      loop(Parent, Debug, State);

    {'EXIT', Parent, _Reason} ->
      gen_server:stop(StreamServerPid),
      ok;

    {system, From, Msg} ->
      sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, State)
  end.
