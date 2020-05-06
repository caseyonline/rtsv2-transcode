-ifndef(rtsv2_master_playlist_processor_hrl).
-define(rtsv2_master_playlist_processor_hrl, 1).

-include("./rtsv2_types.hrl").

-record(hls_master_playlist_processor_config,
        {
          slot_id :: slot_id(),
          profiles :: list(slot_profile()),
          push_details :: list(term())
        }).
-type hls_master_playlist_processor_config() :: #hls_master_playlist_processor_config{}.

-endif.
