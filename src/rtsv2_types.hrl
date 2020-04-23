-ifndef(__rtsv2_types_hrl).
-define(__rtsv2_types_hrl, 1).

-include_lib("id3as_common/include/common.hrl").

-type uuid() ::  <<_:16>>.

-type slot_id() :: uuid().

-type slot_role() :: {primary} | {backup}.

-type profile_name() :: binary_string().

-type ingest_key() :: {ingest_key, slot_id(), slot_role(), binary_string()}.

-endif.
