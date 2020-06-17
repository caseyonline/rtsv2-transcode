-ifndef(rtsv2_bus_messages_hrl).
-define(rtsv2_bus_messages_hrl, 1).

-define(WEBRTC_STREAM_OUTPUT_BUS(IngestKey), {webrtc_stream_output, IngestKey}).

-define(RTMP_EGEST_BUS(IngestKey), {egest_rtmp_bus, IngestKey}).

-define(RTMP_EGEST_BUS(IngestKey, ProfileName), {egest_rtmp_bus, IngestKey, ProfileName}).


-endif.
