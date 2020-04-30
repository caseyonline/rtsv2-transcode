-ifndef(rtsv2_slot_profiles_hrl).
-define(rtsv2_slot_profiles_hrl, 1).

-include("./rtsv2_types.hrl").

-record(enriched_slot_profile,
        { ingest_key :: term()
        , stream_name :: binary_string()
        , profile_name :: binary_string()
        , audio_ssrc_start :: non_neg_integer()
        , video_ssrc_start :: non_neg_integer()
        , bitrate :: integer()
        }).
-type enriched_slot_profile() :: #enriched_slot_profile{}.
%% NO this is not the place for this

-record(hls_master_playlist_processor_config,
        {
          slot_id :: slot_id(),
          profiles :: list(enriched_slot_profile()),
          push_details :: list(term())
        }).
-type hls_master_playlist_processor_config() :: #hls_master_playlist_processor_config{}.

-endif.