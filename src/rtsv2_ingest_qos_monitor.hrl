-ifndef(__rtsv2_ingest_qos_monitor__).
-define(__rtsv2_ingest_qos_monitor__, 1).

-record(rtsv2_ingest_qos_monitor_config,
        { poll_interval_ms = 1000 :: milliseconds()
        , abort_if_no_media_in_ms = 5000 :: milliseconds()
        }).

-endif.
