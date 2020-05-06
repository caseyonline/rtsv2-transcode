-ifndef(__rtsv2_types_hrl).
-define(__rtsv2_types_hrl, 1).

-include_lib("id3as_common/include/common.hrl").

-type uuid() ::  <<_:128>>.

-type slot_id() :: uuid().

-type slot_role() :: {primary} | {backup}.

-type profile_name() :: binary_string().

-type rtmp_short_name() :: binary_string().

-type rtmp_stream_name() :: binary_string().

-type ingest_key() :: {ingest_key, slot_id(), slot_role(), profile_name()}.

-type slot_configuration() :: #{ slotId := slot_id()
                               , slotRole := slot_role()
                               , rtmpShortName := rtmp_short_name()
                               , profiles := list(slot_profile())
                               }.

-type slot_profile() :: #{ profileName := profile_name()
                         , streamName := rtmp_stream_name()
                         , ingestKey := ingest_key()
                         , firstAudioSSRC := integer()
                         , firstVideoSSRC := integer()
                         , bitrate := integer()
                         }.

-endif.
