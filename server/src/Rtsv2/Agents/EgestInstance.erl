-module(rtsv2_agents_egestInstance@foreign).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").
-include_lib("id3as_media/include/transcode.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").
-include_lib("id3as_media/include/stream_sync.hrl").

-include("../../../../src/rtsv2_slot_media_source_publish_processor.hrl").
-include("../../../../src/rtsv2_rtp.hrl").
-include("../../../../src/rtsv2_bus_messages.hrl").



%% foreign exports
-export([ startEgestReceiverFFI/2
        , stopEgestFFI/1
        , getStatsFFI/1
        , setSlotConfigurationFFI/2
        , getSlotConfigurationFFI/1
        ]).


-define(state, ?MODULE).


-define(metadata, rtsv2_agents_streamRelayInstance_metadata).

-record(?metadata,
        { slot_configuration :: slot_configuration()
        }).


startEgestReceiverFFI(EgestKey, UseMediaGateway) ->
  fun() ->

      {ok, _StreamServerPid} =
        webrtc_stream_server:start_link(EgestKey,
                                        #{ stream_module => rtsv2_webrtc_egest_stream_handler
                                         , stream_module_args => [ self(), EgestKey, UseMediaGateway ]
                                         , default_session_config =>
                                             #{ generate_media_channel_ready_messages => true
                                              }
                                         }
                                       ),

      {ok, PortNumber} = rtsv2_webrtc_egest_stream_handler:port_number(EgestKey),

      RtpPortNumber = start_rtmp_bus_workflow(EgestKey),

      {PortNumber, RtpPortNumber}
  end.

stopEgestFFI(EgestKey) ->
  fun() ->
      try
        _ = webrtc_stream_server:stop(EgestKey)
      catch
        C:R ->
          ?SLOG_INFO("Error when closing webrtc_stream_server", #{class => C
                                                                 , reason => R})
      end,
      unit
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
                                  ),

      ok = rtsv2_webrtc_egest_stream_handler:set_slot_configuration(EgestKey, SlotConfiguration),

      ok
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
   , sessionInfo => maps:filter(fun(_, undefined) -> false;
                                   (_, _) -> true
                                end,
                                maps:map(fun webrtc_session_manager_stats_to_purs/2, SessionInfo)
                               )
   }.

webrtc_session_manager_stats_to_purs(_, #{ channels := Channels }) ->
  try
    #{channels => maps:map(fun webrtc_media_channel_stats_to_purs/2, Channels)}
  catch
    _:_ ->
      undefined
  end.

webrtc_media_channel_stats_to_purs(_, #{ frames_dropped_no_return := FramesDroppedNoReturn
                                       , frames_dropped_no_crypto := FramesDroppedNoCrypto
                                       , local_port := LocalPort
                                       , remote_port := RemotePort
                                       , remote_address := RemoteAddress
                                       , octets_received := OctetsReceived
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
   , octetsReceived => OctetsReceived
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


start_rtmp_bus_workflow(EgestKey) ->
  ?INFO("Starting rtp from relay on bus ~p", [{egest_rtmp_bus, EgestKey}]),
  AudioConfig = #audio_transcode_config{
                  input = #frame_spec{
                            profile = #audio_profile{}
                            },
                  outputs = [
                            #named_output{
                                profile_name = aac,
                                frame_spec = #frame_spec{
                                                profile = #audio_profile{
                                                            codec = aac,
                                                            sample_rate = 48000,
                                                            sample_format = s16
                                                            }
                                              }
                              }
                            ]
                },
  Workflow = #workflow{
                name = rtp_to_egest,
                generators = [
                              #generator{name = source
                                        ,display_name = <<"Receive from Stream Relay">>
                                        ,module = rtsv2_rtp_receiver_frame_generator
                                        }
                             ],
                processors = [
                              #processor{name = transcode_audio,
                                         subscribes_to = {source, ?audio_frames},
                                         module = audio_transcode,
                                         config = AudioConfig
                                      },
                              #processor{name = set_audio_stream_id,
                                         subscribes_to = ?previous,
                                         module = set_stream_id,
                                         config = 2
                                      },
                              
                              #processor{name = stream_sync,
                                         subscribes_to = [{source, ?video_frames}, ?previous],
                                         module = stream_sync,
                                         config = #stream_sync_config{ num_streams = 2,
                                                      stream_key = fun (#frame{type = Type, stream_metadata = #stream_metadata{stream_id = StreamId}}) -> {Type, StreamId} end 
                                                  }
                                        },
                              #processor{name = flv,
                                         display_name = <<"FLV Frame Generator">>,
                                         subscribes_to = ?previous,
                                         module = flv_frame_generator
                                        },

                              #processor{name = send_to_bus,
                                        display_name = <<"Send to RTMP Bus">>,
                                        subscribes_to = ?previous,
                                        module = send_to_bus_processor,
                                        config = #send_to_bus_processor_config{consumes = true,
                                                                               bus_name = ?RTMP_EGEST_BUS(EgestKey)}
                                        },

                                                                    
                              #processor{name = sink_other,
                                         subscribes_to = source,
                                         module = dev_null_processor
                                        }
                ]

               },
  {ok, WorkflowPid} = id3as_workflow:start_link(Workflow),
  {ok, WorkflowHandle} = id3as_workflow:workflow_handle(WorkflowPid),

  {ok, Port} = id3as_workflow:ioctl(source, get_port_number, WorkflowHandle),
  Port.
