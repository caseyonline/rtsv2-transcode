-module(rtsv2_rtp_receiver_frame_generator).

-behaviour(workflow_generator).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").

-include_lib("id3as_rtc/include/sdp.hrl"). 
-include_lib("id3as_rtc/include/rtp_sdp.hrl"). 
-include_lib("id3as_rtc/include/rtp_engine.hrl"). 

-export([ init/1
        , handle_info/2
        , ioctl/2
        ]).

-define(state, ?MODULE).

-record(?state,
        { socket :: gen_udp:socket()
        , parse_info :: rtp:parse_info()
        , rtp_engine :: rtp_engine:state()
        }).

%%------------------------------------------------------------------------------
%% Generator API
%%------------------------------------------------------------------------------
init(#generator{}) ->
  {ok, Socket} = gen_udp:open(0, [binary, {recbuf, 100 * 1500}]),

  ParseInfo = rtsv2_rtp_util:build_parse_info(),
  RTPEngineConfig = build_rtp_engine_config(ParseInfo),
  RTPEngine1 = rtp_engine:new(RTPEngineConfig),

  { ok
  , #?state{ socket = Socket
           , parse_info = ParseInfo
           , rtp_engine = RTPEngine1
           }
  }.

handle_info({udp, _Socket, _FromIP, _FromPort, Data}, #?state{ rtp_engine = RtpEngine } = State) ->
  {NewEngine, Frames} = rtp_engine:process_incoming_packet(RtpEngine, Data),
  State2 = State#?state { rtp_engine = NewEngine },
  case Frames of 
    [] ->
      {noreply, State2};
    _ -> {output, Frames, State2}
  end.

ioctl(get_port_number, State = #?state{ socket = Socket }) ->
  {ok, PortNumber} = inet:port(Socket),
  {ok, PortNumber, State}.

%%%

build_rtp_engine_config(ParseInfo) ->
  User = <<"rtsv2">>,
  Host = i_bstr:random_binary_string(8),
  CName = <<User/binary, "@", Host/binary>>,
  IsSecure = false,
  PayloadHandlers = rtp_sdp:supported_payload_handlers(audio, sendonly, 10) 
                  ++ rtp_sdp:supported_payload_handlers(video, sendonly, 20),
  ?INFO("Ingest handlers: ~p", [PayloadHandlers]),
  IngestHandlers = maps:from_list([ {EncodingId, {Module, State}} ||
                                    #payload_handlers{sdp_encoding_id = EncodingId, ingest_handler = {Module, State}} <- PayloadHandlers ]),

  ?INFO("Ingest handlers: ~p", [IngestHandlers]),
  % EgestHandlers = maps:from_list([ {Format, {Module, State}} ||
  %                                  #payload_handlers{frame_format = Format, egest_handler = {Module, State, _Params}} <- PayloadHandlers ]),
  ChannelType = video,
  #rtp_engine_config{ type = ChannelType
                    , display_name = <<"RTP Channel ", CName/binary, "/", (atom_to_binary(ChannelType, utf8))/binary>>
                    , canonical_name = CName
                    , is_secure = IsSecure
                    , parse_info = ParseInfo
                    , time_zero = erlang:monotonic_time(microsecond)
                    , time_synchronization_required = false
                    , ingest_handlers = IngestHandlers
                    , egest_handlers = []
                    }.