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
        , sequence_number_map = #{} :: maps:map(rtp:ssrc(), non_neg_integer()) %% TODO - remove when ABR sequence number rewriting is in place
        }).


port_number(EgestKey) ->
  webrtc_stream_server:call_stream_handler(EgestKey, port_number).


init(_Args = [ ParentPid, EgestKey ]) ->

  process_flag(trap_exit, true),

  {ok, ReceiveSocket} = gen_udp:open(0, [binary, {recbuf, 100 * 1500}]),

  #?state{ parent_pid = ParentPid
         , egest_key = EgestKey
         , receive_socket = ReceiveSocket
         }.


handle_call(port_number, _From, #?state{ receive_socket = ReceiveSocket } = State) ->
  { ok, ReceivePort } = inet:port(ReceiveSocket),
  { reply, {ok, ReceivePort}, State }.


handle_info({udp, ActualSocket, _SenderIP, _SenderPort, Data},
            #?state{ parse_info = ParseInfo, receive_socket = ExpectedSocket, sequence_number_map = SeqMap } = State
           )
  when
    ActualSocket =:= ExpectedSocket ->

  RTP = #rtp{ payload_type = #rtp_payload_type{ encoding_id = EncodingId },
              ssrc = Ssrc} = rtp:parse(avp, Data, ParseInfo),

  LastSeqNum = maps:get(Ssrc, SeqMap, 0),
  NextSeqNum = (LastSeqNum + 1) rem 63365,
  SeqMap2 = maps:put(Ssrc, NextSeqNum, SeqMap),

  %% TODO: PS: SSRC rewriting will ultimately be part of WebRTC when it
  %% supports ABR
  Sequence =
    case EncodingId of
      ?OPUS_ENCODING_ID ->
        #rtp_sequence{ type = audio
                     , codec = opus
                     , rtps = [ RTP#rtp{ ssrc = ?EGEST_AUDIO_SSRC
                                       , sequence_number = NextSeqNum } ]
                     };
      ?H264_ENCODING_ID ->
        #rtp_sequence{ type = video
                     , codec = h264
                     , rtps = [ RTP#rtp{ ssrc = ?EGEST_VIDEO_SSRC
                                       , sequence_number = NextSeqNum } ]
                     }
    end,

  { broadcast, Sequence, State#?state{sequence_number_map = SeqMap2} };

handle_info({'EXIT', MaybeParentPid, _Reason }, #?state{ parent_pid = ParentPid } = State) when MaybeParentPid =:= ParentPid ->
  { stop, State }.
