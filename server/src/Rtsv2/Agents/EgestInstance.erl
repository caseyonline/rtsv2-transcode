-module(rtsv2_agents_egestInstance@foreign).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").
-include("../../../../src/rtsv2_slot_media_source_publish_processor.hrl").
-include("../../../../src/rtsv2_rtp.hrl").


%% foreign exports
-export([ startEgestReceiverFFI/1
        , getStatsFFI/1
        , setSlotConfigurationFFI/2
        , getSlotConfigurationFFI/1
        ]).


-define(state, ?MODULE).


-define(metadata, rtsv2_agents_streamRelayInstance_metadata).

-record(?metadata,
        { slot_configuration :: rtsv2_slot_configuration:slot_configuration()
        }).


startEgestReceiverFFI(EgestKey) ->
  fun() ->

      {ok, _StreamServerPid} =
        webrtc_stream_server:start_link(EgestKey,
                                        #{ stream_module => rtsv2_webrtc_egest_stream_handler
                                         , stream_module_args => [ self(), EgestKey ]
                                         }
                                       ),

      {ok, PortNumber} = rtsv2_webrtc_egest_stream_handler:port_number(EgestKey),

      PortNumber
  end.

getStatsFFI(EgestKey) ->
  fun() ->
      {ok, Stats} = webrtc_stream_server:get_stats(EgestKey),

      webrtc_stream_server_stats_to_purs(Stats)
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

webrtc_stream_server_stats_to_purs(#{ server_id := Id
                                    , session_count := SessionCount
                                    , session_info := SessionInfo}) ->
  #{ serverId => Id
   , sessionCount => SessionCount
   , sessionInfo => maps:map(fun webrtc_session_manager_stats_to_purs/2, SessionInfo)}.

webrtc_session_manager_stats_to_purs(_, #{ channels := Channels }) ->
  #{channels => maps:map(fun webrtc_media_channel_stats_to_purs/2, Channels)}.

webrtc_media_channel_stats_to_purs(_, #{ frames_dropped_no_return := FramesDroppedNoReturn
                                       , frames_dropped_no_crypto := FramesDroppedNoCrypto
                                       , local_port := LocalPort
                                       , remote_port := RemotePort
                                       , remote_address := RemoteAddress
                                       , octets_sent := OctetsSent
                                       , packets_sent := PacketsSent
                                       , incoming_ssrc := IncomingSsrc
                                       , received_plis := ReceivedPlis
                                       , receiver_info := ReceivedInfo
                                       }) ->
  #{ framesDroppedNoReturn => FramesDroppedNoReturn
   , framesDroppedNoCrypto => FramesDroppedNoCrypto
   , localPort => LocalPort
   , remotePort => RemotePort
   , remoteAddress => list_to_binary(inet:ntoa(RemoteAddress))
   , octetsSent => OctetsSent
   , packetsSent => PacketsSent
   , incomingSsrc => IncomingSsrc
   , receivedPlis => ReceivedPlis
   , receiverInfo => maps:map(fun rtcp_reception_stats_to_purs/2, ReceivedInfo)
   }.

rtcp_reception_stats_to_purs(_, #{ lost_fraction := LostFraction
                                 , lost_total := LostTotal
                                 , sequence_epoch := SequenceEpoch
                                 , sequence_latest := SequenceLatest
                                 , interarrival_jitter := InterarrivalJitter
                                 }) ->

  #{ lostFraction => LostFraction
   , lostTotal => LostTotal
   , sequenceEpoch => SequenceEpoch
   , sequenceLatest => SequenceLatest
   , interarrivalJitter => InterarrivalJitter
   }.
