-module(rtsv2_alerts@foreign).

-export([ mapForeign/1
        ]).

mapForeign(LogEvent = #{meta := #{ time := Time
                                 , pid := Pid }}) ->
  #{ initialReport => Time div 1000
   , lastReport => Time div 1000
   , repeatCount => 0
   , alert => mapToAlert(LogEvent)
   , metadata => maybe_rtsv2_metadata(LogEvent)
   , source => maybe_logging_source(LogEvent)
   , pid => list_to_binary(pid_to_list(Pid))
   }.

mapToAlert(_LogEvent = #{meta := #{ alertId := ingestStarted } } ) ->
  {ingestStarted};

mapToAlert(_LogEvent = #{ meta := #{ alertId := invalidVideoFormat }
                        , msg := {_FormatString, [CodecId]}} ) ->
  {ingestFailed, {invalidVideoCodec, CodecId}};

mapToAlert(LogEvent) ->
  {genericAlert, #{text => iolist_to_binary(logger_formatter:format(LogEvent, #{template =>  [level, ": ", msg]}))
                  }}.

maybe_rtsv2_metadata(#{meta := #{rtsv2 := Metadata}}) ->
  {just, Metadata};

maybe_rtsv2_metadata(#{meta := #{workflow := #{workflow_tags := #{profile_metadata := ProfileMetadata }}}}) ->
  {just, {perProfile, ProfileMetadata}};

maybe_rtsv2_metadata(_) ->
  {nothing}.

maybe_logging_source(#{meta := #{ mfa := {Module, Function, _}
                                , line := Line
                                }}) ->
  {just, #{ module => Module
          , function => Function
          , line => Line
          }};

maybe_logging_source(_) ->
  {nothing}.
