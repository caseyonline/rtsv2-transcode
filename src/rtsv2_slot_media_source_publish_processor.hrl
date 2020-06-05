-ifndef(__rtsv2_slot_media_source_publis_processor_hrl__).
-define(__rtsv2_slot_media_source_publis_processor_hrl__, 1).

-include("./rtsv2_types.hrl").

-record(rtsv2_slot_media_source_publish_processor_config,
        { slot_name :: term()
        , slot_role :: term()
        , slot_configuration = undefined :: slot_configuration()
        }).
-type rtsv2_slot_media_source_publish_processor_config() :: #rtsv2_slot_media_source_publish_processor_config{}.

-endif.
