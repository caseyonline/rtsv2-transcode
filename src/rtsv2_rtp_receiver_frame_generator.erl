-module(rtsv2_rtp_receiver_frame_generator).

-behaviour(workflow_generator).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").

-include_lib("id3as_rtc/include/sdp.hrl"). 
-include_lib("id3as_rtc/include/rtp_sdp.hrl"). 
-include_lib("id3as_rtc/include/rtp_engine.hrl"). 
-include("./rtsv2_rtp.hrl").

-export([ init/1
        , handle_info/2
        , ioctl/2
        ]).

-define(state, ?MODULE).

-record(?state,
        { socket :: gen_udp:socket()
        , parse_info :: rtp:parse_info()
        , h264_ingest_state = #{} :: maps:map(rtp:ssrc(), rtp_h264_ingest:state())
        , opus_ingest_state = #{} :: maps:map(rtp:ssrc(), rtp_opus_ingest:state())
        , wrap_state = #{} :: maps:map(rtp:ssrc(), rtp_timestamp_wrap:state())
        }).

%%------------------------------------------------------------------------------
%% Generator API
%%------------------------------------------------------------------------------
init(#generator{}) ->
  {ok, Socket} = gen_udp:open(0, [binary, {recbuf, 100 * 1500}]),

  ParseInfo = rtsv2_rtp_util:build_parse_info(),

  { ok
  , #?state{ socket = Socket
           , parse_info = ParseInfo
           }
  }.

handle_info({udp, _Socket, _FromIP, _FromPort, Data},
            #?state{ parse_info = ParseInfo
                   , h264_ingest_state = RtpH264StateMap
                   , opus_ingest_state = RtpOpusStateMap
                   , wrap_state = WrapState
                   } = State) ->
  #rtp{ ssrc = SSRC, payload_type = #rtp_payload_type{encoding_id = EncodingId}, timestamp = Timestamp, processing_metadata = ProcessingMetadata } = RTP = rtp:parse(avp, Data, ParseInfo),
  {Frames, State2} = case EncodingId of
    ?H264_ENCODING_ID ->
      H264WrapState = maps:get(SSRC, WrapState, rtp_timestamp_wrap:new(90000)), 
      {H264WrapState2,_,ExtendedTimestamp} = rtp_timestamp_wrap:compute_extended_timestamp(H264WrapState, Timestamp),
      RTP2 = RTP#rtp { processing_metadata = ProcessingMetadata#rtp_processing_metadata{ frame_time = ExtendedTimestamp } },

      RtpH264State = maps:get(SSRC, RtpH264StateMap, rtp_h264_ingest:new()),
      {RtpH264State2, Frames1} = rtp_h264_ingest:step(RtpH264State, RTP2),

      {Frames1, State#?state{ h264_ingest_state = maps:put(SSRC, RtpH264State2, RtpH264StateMap), wrap_state = maps:put(SSRC, H264WrapState2, WrapState)}};

    ?OPUS_ENCODING_ID ->
      OpusWrapState = maps:get(SSRC, WrapState, rtp_timestamp_wrap:new(48000)),
      {OpusWrapState2,_,ExtendedTimestamp} = rtp_timestamp_wrap:compute_extended_timestamp(OpusWrapState, Timestamp),
      RTP2 = RTP#rtp { processing_metadata = ProcessingMetadata#rtp_processing_metadata{ frame_time = round(ExtendedTimestamp * 90/48) } },

      RtpOpusState = maps:get(SSRC, RtpOpusStateMap, rtp_opus_ingest:new()),
      {RtpOpusState2, Frames1} = rtp_opus_ingest:step(RtpOpusState, RTP2),
      
      {Frames1, State#?state{ opus_ingest_state = maps:put(SSRC, RtpOpusState2, RtpOpusStateMap), wrap_state = maps:put(SSRC, OpusWrapState2, WrapState)}};

    _ ->
      {[], State}
  end,
  Frames2 = [ F#frame{ stream_metadata = F#frame.stream_metadata#stream_metadata{ stream_id = SSRC } } || F <- Frames],
  case Frames2 of 
    [] ->
      {noreply, State2};
    _ -> {output, Frames2, State2}
  end.

ioctl(get_port_number, State = #?state{ socket = Socket }) ->
  {ok, PortNumber} = inet:port(Socket),
  {ok, PortNumber, State}.
