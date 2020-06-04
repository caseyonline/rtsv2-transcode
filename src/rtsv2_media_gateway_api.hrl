-ifndef(rtsv2_media_gateway_api_hrl).
-define(rtsv2_media_gateway_api_hrl, 1).

-include_lib("id3as_rtc/include/srtp_crypto.hrl").




-record(media_gateway_test_stream_element_config,
        { payload_type_id :: rtp:encoding_id()
        , input_ssrc :: rtp:ssrc()
        , target_ip :: inet:ip_address()
        , target_port :: inet:port_number()
        }).
-type media_gateway_test_stream_element_config() :: #media_gateway_test_stream_element_config{}.


-record(media_gateway_test_egest_client_config,
        { audio :: media_gateway_test_stream_element_config()
        , video :: media_gateway_test_stream_element_config()
        }).
-type media_gateway_test_egest_client_config() :: #media_gateway_test_egest_client_config{}.







-record(media_gateway_stream_element_config,
        { media_socket :: gen_udp:socket()
        , egest_crypto :: srtp_crypto_params()
        , cname :: binary_string()
        , payload_type_id :: rtp:encoding_id()
        , input_ssrc :: rtp:ssrc()
        }).
-type media_gateway_stream_element_config() :: #media_gateway_stream_element_config{}.


-record(media_gateway_egest_client_config,
        { audio :: media_gateway_stream_element_config()
        , video :: undefined | media_gateway_stream_element_config()
        }).
-type media_gateway_egest_client_config() :: #media_gateway_egest_client_config{}.


-record(media_gateway_event,
        { details :: media_gateway_event_details()
        }).

-record(media_gateway_client_synchronization_established_event,
        { rtp_timestamp :: non_neg_integer()
        }).
-type media_gateway_client_synchronization_established_event() :: #media_gateway_client_synchronization_established_event{}.

-record(media_gateway_client_subscription_switched_event,
        { audio_ssrc :: rtp:ssrc()
        , video_ssrc :: undefined | rtp:ssrc()
        }).
-type media_gateway_client_subscription_switched_event() :: #media_gateway_client_subscription_switched_event{}.

-record(media_gateway_client_statistics_updated_event,
        { audio_packets_sent :: non_neg_integer()
        , audio_octets_sent :: non_neg_integer()
        , video_packets_sent :: non_neg_integer()
        , video_octets_sent :: non_neg_integer()
        }).
-type media_gateway_client_statistics_updated_event() :: #media_gateway_client_statistics_updated_event{}.

-record(media_gateway_client_add_failed_event,
        { reason :: egest_not_present | already_present | stream_is_audio_only | no_capacity
        }).
-type media_gateway_client_add_failed_event() :: #media_gateway_client_add_failed_event{}.

-record(media_gateway_client_subscription_update_failed_event,
        { reason :: not_present
        }).
-type media_gateway_client_subscription_update_failed_event() :: #media_gateway_client_subscription_update_failed_event{}.

-type media_gateway_event_details() ::
        media_gateway_client_synchronization_established_event()
      | media_gateway_client_subscription_switched_event()
      | media_gateway_client_statistics_updated_event()
      | media_gateway_client_add_failed_event()
      | media_gateway_client_subscription_switched_event().

-endif.
