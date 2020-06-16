-ifndef(__rtsv2_ingest_qos_monitor__).
-define(__rtsv2_ingest_qos_monitor__, 1).

-include("./rtsv2_types.hrl").

-record(rtsv2_ingest_qos_monitor_config,
        { ingestInstanceConfig :: maps:map()
        , slotProfile :: llnw_slot_profile()
        }).

-endif.
