-ifndef(rtsv2_internal_hls_writer_hrl).
-define(rtsv2_internal_hls_writer_hrl, 1).

-record(rtsv2_internal_hls_writer_config, {
          collision_protection_mode = none :: none | {directory_prefix, binary_string()},
          post_url :: undefined | binary_string(),
          auth :: {binary_string(), binary_string()}, %% basic auth username/pwd
          target_segment_duration = 10 :: integer(),
          max_entries_per_directory = 500 :: integer(),
          max_playlist_length = 10 :: integer(),
          playlist_name = <<"playlist.m3u8">> :: binary_string(),
          version_string = <<"legacy">> :: binary_string() %% Please set from your app guys
         }).
-type rtsv2_internal_hls_writer_config() :: #rtsv2_internal_hls_writer_config{}.

-endif.