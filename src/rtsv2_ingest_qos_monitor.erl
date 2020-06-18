-module(rtsv2_ingest_qos_monitor).

-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/bitrate_monitor.hrl").

-include("./rtsv2_types.hrl").
-include("./rtsv2_ingest_qos_monitor.hrl").

-behaviour(workflow_processor).

-export([
         spec/1,
         initialise/1,
         process_input/2,
         handle_info/2,
         flush/1,
         ioctl/2
        ]).

-define(state, ?MODULE).
-record(?state,
        { poll_interval_ms :: milliseconds()
        , abort_if_no_media_in_ms :: milliseconds()
        , last_audio_frame_seen :: undefined | milliseconds()
        , last_video_frame_seen :: undefined | milliseconds()
        , qosAverageBitrateLowWatermark :: non_neg_integer()
        , qosAverageBitrateHighWatermark :: non_neg_integer()
        , qosPeakBitrateLowWatermark :: non_neg_integer()
        , qosPeakBitrateHighWatermark :: non_neg_integer()
        , configuredBitrate :: non_neg_integer()
        , slotProfile :: llnw_slot_profile()
        , mode :: audio_only | audio_video
        }).

%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{
     consumes = [?frames, ?bitrate_info_groups],
     supports_synchronous_mode = true
    }.

initialise(_Processor = #processor{config = #rtsv2_ingest_qos_monitor_config{ ingestInstanceConfig = IngestInstanceConfig
                                                                            , slotProfile = SlotProfile
                                                                            }}) ->

  #{ qosPollIntervalMs := QosPollIntervalMs
   , abortIfNoMediaMs := AbortIfNoMediaMs
   , qosAverageBitrateLowWatermark := QosAverageBitrateLowWatermark
   , qosAverageBitrateHighWatermark := QosAverageBitrateHighWatermark
   , qosPeakBitrateLowWatermark := QosPeakBitrateLowWatermark
   , qosPeakBitrateHighWatermark := QosPeakBitrateHighWatermark
   } = IngestInstanceConfig,

  #{ audioBitrate := AudioBitrate
   , videoBitrate := VideoBitrate } = SlotProfile,

  ConfiguredBitrate = AudioBitrate + VideoBitrate,

  erlang:send_after(QosPollIntervalMs, self(), poll),

  {ok, #?state{ poll_interval_ms = QosPollIntervalMs
              , abort_if_no_media_in_ms = AbortIfNoMediaMs
              , qosAverageBitrateLowWatermark = trunc(ConfiguredBitrate * QosAverageBitrateLowWatermark)
              , qosAverageBitrateHighWatermark = trunc(ConfiguredBitrate * QosAverageBitrateHighWatermark)
              , qosPeakBitrateLowWatermark = trunc(ConfiguredBitrate * QosPeakBitrateLowWatermark)
              , qosPeakBitrateHighWatermark = trunc(ConfiguredBitrate * QosPeakBitrateHighWatermark)
              , slotProfile = SlotProfile
              , mode = case VideoBitrate of
                         0 -> audio_only;
                         _ -> audio_video
                       end
              }}.

process_input(#frame{type = audio}, State) ->
  {ok, State#?state{last_audio_frame_seen = ?vm_now_ms}};

process_input(#frame{type = video, dts = _Dts, pts = _Pts}, State) ->
  {ok, State#?state{last_video_frame_seen = ?vm_now_ms}};

process_input(#frame{}, State) ->
  {ok, State};

process_input(#bitrate_info_group{name = average_bitrate_monitor,
                                  infos = Infos},
              State = #?state{ qosAverageBitrateLowWatermark = QosAverageBitrateLowWatermark
                             , qosAverageBitrateHighWatermark = QosAverageBitrateHighWatermark
                             }) ->

  Average = bps(Infos),

  if
    Average > QosAverageBitrateHighWatermark ->
      ?SLOG_ERROR( "Average bitrate exceeded high watermark"
                 , #{ value => Average
                    , threshold => QosAverageBitrateHighWatermark }
                 , #{ alertId => ingestWarning
                    , reason => bitrateExceeded
                    , bitrateType => average
                    , watermarkExceeded => high });
    Average > QosAverageBitrateLowWatermark ->
      ?SLOG_ERROR( "Average bitrate exceeded low watermark"
                 , #{ value => Average
                    , threshold => QosAverageBitrateLowWatermark}
                 , #{ alertId => ingestWarning
                    , reason => bitrateExceeded
                    , bitrateType => average
                    , watermarkExceeded => low });
    ?otherwise -> ok
  end,

  {ok, State};

process_input(#bitrate_info_group{name = peak_bitrate_monitor,
                                  infos = Infos},
              State = #?state{ qosPeakBitrateLowWatermark = QosPeakBitrateLowWatermark
                             , qosPeakBitrateHighWatermark = QosPeakBitrateHighWatermark
                             }) ->

  Peak = bps(Infos),

  if
    Peak > QosPeakBitrateHighWatermark ->
      ?SLOG_ERROR( "Peak bitrate exceeded high watermark"
                 , #{ value => Peak
                    , threshold => QosPeakBitrateHighWatermark }
                 , #{ alertId => ingestWarning
                    , reason => bitrateExceeded
                    , bitrateType => peak
                    , watermarkExceeded => high });
    Peak > QosPeakBitrateLowWatermark ->
      ?SLOG_ERROR( "Peak bitrate exceeded low watermark"
                 , #{ value => Peak
                    , threshold => QosPeakBitrateLowWatermark}
                 , #{ alertId => ingestWarning
                    , reason => bitrateExceeded
                    , bitrateType => peak
                    , watermarkExceeded => low });
    ?otherwise -> ok
  end,

  {ok, State}.

handle_info(poll, State = #?state{ poll_interval_ms = PollInterval
                                 , abort_if_no_media_in_ms = AbortMs
                                 , last_audio_frame_seen = LastAudioSeen
                                 , last_video_frame_seen = LastVideoSeen
                                 , mode = Mode
                                 }) ->
  Now = ?vm_now_ms,

  if
    Now - LastAudioSeen > AbortMs ->
      ?SLOG_WARNING("Ingest workflow abort since no audio seen", #{abort_ms => AbortMs}, #{ alertId => ingestFailed
                                                                                          , reason => noAudioReceived }),
      {stop, {error, nomedia}, State};

    Now - LastVideoSeen > AbortMs, Mode == audio_video ->
      ?SLOG_WARNING("Ingest workflow abort since no video seen", #{abort_ms => AbortMs}, #{ alertId => ingestFailed
                                                                                          , reason => noVideoReceived }),
      {stop, {error, nomedia}, State};

    LastVideoSeen /= undefined, Mode == audio_only ->
      ?SLOG_WARNING("Ingest audio-only workflow abort since video seen", #{}, #{ alertId => ingestFailed
                                                                               , reason => videoReceivedOnAudioOnlyProfile }),
      {stop, {error, video_on_audio_only}, State};

    ?otherwise ->
      erlang:send_after(PollInterval, self(), poll),
      {noreply, State}
  end.

flush(State) ->
  {flush_complete, State}.

ioctl(_, State) ->
  {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
bps(Infos) ->
  lists:foldl(fun(#bitrate_info{bytes_per_sec = Bps}, Acc) -> Bps + Acc end, 0, Infos) * 8.
