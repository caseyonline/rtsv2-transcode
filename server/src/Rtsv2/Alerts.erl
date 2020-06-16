-module(rtsv2_alerts@foreign).

-export([ mapForeign/1
        ]).

mapForeign(LogEvent = #{meta := #{ time := Time
                                 , pid := Pid }}) ->

  #{ initialReport => Time div 1000
   , lastReport => Time div 1000
   , repeatCount => 0
   , alert => mapToAlert(LogEvent)
   , context => maybe_rtsv2_context(LogEvent)
   , source => maybe_logging_source(LogEvent)
   , pid => list_to_binary(pid_to_list(Pid))
   }.

mapToAlert(_LogEvent = #{ meta := #{ alertId := ingestStarted } } ) ->
  {ingestStarted};

mapToAlert(_LogEvent = #{ meta := #{ alertId := ingestBitrateExceeded
                                   , bitrateType := BitrateType
                                   , watermarkExceeded := WatermarkType }
                        , msg := {report, #{ value := Value
                                           , threshold := Watermark }}
                        } ) ->
  {ingestWarning, {bitrateExceeded, #{ bitrateType => case BitrateType of
                                                        average -> {average};
                                                        peak -> {peak}
                                                      end
                                     , watermarkType => case WatermarkType of
                                                          high -> {high};
                                                          low -> {low}
                                                        end
                                     , value => Value
                                     , threshold => Watermark }}};

mapToAlert(_LogEvent = #{ meta := #{ alertId := invalidVideoFormat }
                        , msg := {_FormatString, [CodecId]}} ) ->
  {ingestFailed, {invalidVideoCodec, CodecId}};

mapToAlert(LogEvent = #{ meta := #{ alertId := lsrsFailure } }) ->
  {lSRSFailed, #{reason => log_event_to_string(LogEvent)
                }};

mapToAlert(LogEvent) ->
  {genericAlert, #{text => log_event_to_string(LogEvent)
                  }}.

maybe_rtsv2_context(#{meta := #{rtsv2_context := Context}}) ->
  {just, Context};

maybe_rtsv2_context(#{meta := #{workflow := #{workflow_tags := #{profile_context := ProfileContext }}}}) ->
  {just, {perProfile, ProfileContext}};

maybe_rtsv2_context(_) ->
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

log_event_to_string(LogEvent) ->
  iolist_to_binary(logger_formatter:format(LogEvent, #{template =>  [level, ": ", msg]})).
