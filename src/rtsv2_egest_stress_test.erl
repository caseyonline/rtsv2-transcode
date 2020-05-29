-module(rtsv2_egest_stress_test).


-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_common/include/id3as_message_bus.hrl").
-include("./rtsv2_media_gateway_api.hrl").
-include("./rtsv2_rtp.hrl").


-export([ start_test_client/0
        , start_test_client/1
        , start_test_clients/2
        ]).


start_test_clients(From, To) ->
  lists:foreach(fun(ClientId) ->
                    timer:sleep(2),
                    start_test_client(ClientId)
                end,
                lists:seq(From, To)
               ).

start_test_client() ->
  start_test_client(1).

start_test_client(ClientId) ->
  SlotId = 1,
  SlotRole = primary,

  Config =
    #media_gateway_test_egest_client_config{ audio = #media_gateway_test_stream_element_config{ payload_type_id = ?OPUS_ENCODING_ID
                                                                                              , input_ssrc = ?make_audio_ssrc(16, 0)
                                                                                              , target_ip = {127, 0, 0, 1}
                                                                                              , target_port = 4242
                                                                                              }
                                           , video = #media_gateway_test_stream_element_config{ payload_type_id = ?H264_ENCODING_ID
                                                                                              , input_ssrc = ?make_video_ssrc(16, 0)
                                                                                              , target_ip = {127, 0, 0, 1}
                                                                                              , target_port = 4242
                                                                                              }
                                           },

  rtsv2_media_gateway_api:add_test_egest_client(SlotId, SlotRole, ClientId, Config).
