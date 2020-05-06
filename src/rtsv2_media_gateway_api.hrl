-ifndef(rtsv2_media_gateway_api_hrl).
-define(rtsv2_media_gateway_api_hrl, 1).

-include_lib("id3as_rtc/include/srtp_crypto.hrl").


-record(media_gateway_stream_element_config,
        { media_socket :: gen_udp:socket()
        , egest_crypto :: srtp_crypto_params()
        , cname :: binary_string()
        }).
-type media_gateway_stream_element_config() :: #media_gateway_stream_element_config{}.


-record(media_gateway_egest_client_config,
        { audio :: media_gateway_stream_element_config()
        , video :: media_gateway_stream_element_config()
        }).
-type media_gateway_egest_client_config() :: #media_gateway_egest_client_config{}.

-endif.
