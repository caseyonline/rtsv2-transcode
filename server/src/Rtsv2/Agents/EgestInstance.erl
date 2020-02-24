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
