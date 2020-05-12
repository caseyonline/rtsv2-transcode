-ifndef(rtsv2_media_gateway_api_hrl).
-define(rtsv2_media_gateway_api_hrl, 1).

-include_lib("id3as_rtc/include/srtp_crypto.hrl").


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
        , video :: media_gateway_stream_element_config()
        }).
-type media_gateway_egest_client_config() :: #media_gateway_egest_client_config{}.


-record(media_gateway_event,
        { details :: media_gateway_event_details()
        }).

-record(media_gateway_client_synchronization_established_event,
        { rtp_timestamp :: non_neg_integer()
        }).
-type media_gateway_client_synchronization_established_event() :: #media_gateway_client_synchronization_established_event{}.

-type media_gateway_event_details() :: media_gateway_client_synchronization_established_event().

-endif.
