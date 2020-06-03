-ifndef(__rtsv2_hls_segment__).
-define(__rtsv2_hls_segment__, 1).


-record(rtsv2_hls_segment_workflow_config, {
  slot_id :: slot_id(),
  slot_profile :: maps:map(),
  push_details :: maps:map(),
  audio_only :: boolean()
}).
-type rtsv2_hls_segment_workflow_config() :: #rtsv2_hls_segment_workflow_config{}.

-endif.