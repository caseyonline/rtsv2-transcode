-ifndef(rtsv2_webrtc_hrl).
-define(rtsv2_webrtc_hrl, 1).

-include("./rtsv2_types.hrl").

-record(rtsv2_webrtc_session_handler_config,
        { session_id :: binary_string()
        , cname :: binary_string()
        , media_gateway_client_id :: non_neg_integer()
        , slot_id :: slot_id()
        , slot_role :: slot_role()
        , audio_only :: boolean()
        , profiles :: list(slot_profile())
        , active_profile_names :: list(binary_string())
        , web_socket :: pid()
        , audio_ssrc :: rtp:ssrc()
        , video_ssrc :: rtp:ssrc()
        , use_media_gateway :: boolean()

        }).
-type rtsv2_webrtc_session_handler_config() :: #rtsv2_webrtc_session_handler_config{}.

-endif.
