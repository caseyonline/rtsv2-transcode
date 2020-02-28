-module(rtsv2_rtp_util).

-include_lib("id3as_rtc/include/rtp.hrl").
-include("./rtsv2_rtp.hrl").

-export([ build_parse_info/0
        ]).

build_parse_info() ->
  #rtp_parse_info{ extension_map = #{}
                 , payload_map =
                     #{ ?OPUS_ENCODING_ID => #rtp_payload_type{ encoding_id = ?OPUS_ENCODING_ID, encoding_name = <<"opus">>, clock_rate = 48000, optional_encoding_parameters = <<"2">> }
                      , ?H264_ENCODING_ID => #rtp_payload_type{ encoding_id = ?H264_ENCODING_ID, encoding_name = <<"h264">>, clock_rate = 90000 }
                      }
                 }.
