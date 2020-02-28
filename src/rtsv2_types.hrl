-ifndef(__rtsv2_types_hrl).
-define(__rtsv2_types_hrl, 1).

-include_lib("id3as_common/include/common.hrl").

-type slot_id() :: non_neg_integer().

-type slot_role() :: {primary} | {backup}.

-type profile_name() :: binary_string().

-type ingest_key() :: {ingest_key, slot_id(), slot_role(), binary_string()}.

-endif.
