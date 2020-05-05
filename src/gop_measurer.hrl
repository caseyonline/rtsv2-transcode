-ifndef(__gop_measurer__).
-define(__gop_measurer__, 1).

-record(gop_measurement, {
    duration :: integer(),

    stream_id :: integer(),
    source_id :: term(),
    profile_name :: term()
}).

-type gop_measurement() :: #gop_measurement{}.

-define(gop_measurements, #named_ets_spec{name = list_to_atom("gop_measurements"),
                                          spec = ?wildcard_by_name(gop_measurement)}).

-define(gop_measurements_with_stream_and_sourceid(StreamId, SourceId), #named_ets_spec{name = list_to_atom("gop_measurements_with_source_id: " ++ ??SourceId ++ " and_stream_id: " ++ ?StreamId),
                                                                                       spec = ?wildcard_by_name(gop_measurement)#gop_measurement{ stream_id = StreamId, source_id = SourceId }}).

-define(gop_measurements_with_sourceid(SourceId), #named_ets_spec{name = list_to_atom("gop_measurements_with_source_id: " ++ ??SourceId),
                                                                                       spec = ?wildcard_by_name(gop_measurement)#gop_measurement{ source_id = SourceId }}).


-endif.
