-module(rtsv2_agents_ingestStats@foreign).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").

-export([
         getStatsImpl/0
        ]).

getStatsImpl() ->
  fun() ->
      WorkflowSpec = [{#workflow_context{ref = '$1',
                                         name = {rtmp_ingest_handler, '$2'},
                                         _ = '_'},
                       [],
                       [{{'$1', '$2'}}]}],

      Refs = ets:select(id3as_workflows, WorkflowSpec),

      lists:filtermap(fun({Ref, StreamAndVariant}) ->
                          MetricsSpec = [{#workflow_node_status{id =
                                                                  #workflow_node_id{ref = Ref,
                                                                                    path = ['$1', {rtmp_ingest_handler, {streamAndVariant,'_','_'}}]},
                                                                status = '$2',
                                                                _ = '_'},
                                          [{'orelse',
                                            {'==', '$1', source_bitrate_monitor},
                                            {'orelse',
                                             {'==', '$1', rtmp_ingest},
                                             {'==', '$1', source_frame_meter}}}],
                                          ['$2']}],

                          Statuses = ets:select(id3as_workflow_node_metrics, MetricsSpec),

                          Initial = #{ timestamp => ?now_ms
                                     , streamAndVariant => StreamAndVariant},

                          Output = lists:foldl(fun(#status{module = frame_flow_meter,
                                                           metrics = Metrics}, Acc) ->
                                                   Acc#{frameFlowMeterMetrics => frame_flow_metrics_to_purs(Metrics)};
                                                  (#status{module = stream_bitrate_monitor,
                                                           metrics = Metrics}, Acc) ->
                                                   Acc#{streamBitrateMetrics => stream_bitrate_metrics_to_purs(Metrics)};
                                                  (#status{module = rtmp_push_ingest_generator,
                                                           metrics = Metrics}, Acc) ->
                                                   Acc#{rtmpIngestMetrics => rtmp_metrics_metrics_to_purs(Metrics)}
                                               end,
                                               Initial,
                                               Statuses),
                          Fields = maps:size(Output),
                          if
                            Fields == 5 -> {true, Output};
                            ?otherwise -> false
                          end
                      end,
                      Refs)
  end.

rtmp_metrics_metrics_to_purs(Metrics) ->

  lists:foldl(fun(#text_metric{name = remote_ip, value = Value}, Acc) ->
                  Acc#{remoteIp => Value};
                 (#gauge_metric{name = remote_port, value = Value}, Acc) ->
                  Acc#{remotePort => Value};
                 (#counter_metric{name = bytes_read, value = Value}, Acc) ->
                  Acc#{bytesRead => Value};
                 (_, Acc) ->
                  Acc
              end,
              #{},
              Metrics).

frame_flow_metrics_to_purs(Metrics) ->

  Streams = lists:foldl(fun(#counter_metric{name = frame_count,
                                            value = Value,
                                            tags = Tags}, Acc) ->
                            update(frameCount, Value, Tags, Acc);
                           (#counter_metric{name = byte_count,
                                            value = Value,
                                            tags = Tags}, Acc) ->
                            update(byteCount, Value, Tags, Acc);
                           (#counter_metric{name = last_dts,
                                            value = Value,
                                            tags = Tags}, Acc) ->
                            update(lastDts, Value, Tags, Acc);
                           (#counter_metric{name = last_pts,
                                            value = Value,
                                            tags = Tags}, Acc) ->
                            update(lastPts, Value, Tags, Acc);
                           (#counter_metric{name = last_capture_ms,
                                            value = Value,
                                            tags = Tags}, Acc) ->
                            update(lastCaptureMs, to_maybe(Value), Tags, Acc);
                           (#text_metric{name = codec,
                                         value = Value,
                                         tags = Tags}, Acc) ->
                            update(codec, to_maybe(case Value of
                                                     undefined -> undefined;
                                                     _ -> atom_to_binary(Value, utf8)
                                                   end), Tags, Acc);
                           (_, Acc) ->
                            Acc
                        end,
                        #{},
                        Metrics),

  #{perStreamMetrics => [maps:put(metrics, StreamMetrics, tags_to_stream(Tags))
                         || {Tags, StreamMetrics} <- maps:to_list(Streams)]}.

stream_bitrate_metrics_to_purs(Metrics) ->

  Outer = lists:foldl(fun(#gauge_metric{name = window_size, value = WindowSize}, Acc) ->
                          Acc#{windowSize => WindowSize};
                         (#gauge_metric{name = notification_frequency, value = NotificationFrequency}, Acc) ->
                          Acc#{notificationFrequency => NotificationFrequency};
                         (_, Acc) ->
                          Acc
                      end,
                      #{},
                      Metrics),

  Streams = lists:foldl(fun(#counter_metric{name = frame_count,
                                            value = Value,
                                            tags = Tags}, Acc) ->
                            update(frameCount, Value, Tags, Acc);
                           (#gauge_metric{name = packets_per_second,
                                          value = Value,
                                          tags = Tags}, Acc) ->
                            update(packetsPerSecond, Value, Tags, Acc);
                           (#gauge_metric{name = bitrate,
                                          value = Value,
                                          tags = Tags}, Acc) ->
                            update(bitrate, Value, Tags, Acc);
                           (#gauge_metric{name = average_packet_size,
                                          value = Value,
                                          tags = Tags}, Acc) ->
                            update(averagePacketSize, Value, Tags, Acc);
                           (_, Acc) ->
                            Acc
                        end,
                        #{},
                        Metrics),

  Outer#{perStreamMetrics => [maps:put(metrics, StreamMetrics, tags_to_stream(Tags))
                              || {Tags, StreamMetrics} <- maps:to_list(Streams)]}.

tags_to_stream(Tags) ->
  lists:foldl(fun({profile_name, Name}, Acc) ->
                  Acc#{profileName => atom_to_binary(Name, utf8)};
                 ({stream_id, StreamId}, Acc) ->
                  Acc#{streamId => binary_to_integer(StreamId)};
                 ({frame_type, FrameType}, Acc) ->
                  Acc#{frameType => {case FrameType of
                                       video -> 'video';
                                       audio -> 'audio';
                                       subtitles -> 'subtitles';
                                       program_details -> 'programDetails';
                                       pcr -> 'pCR'
                                     end}}
              end,
              #{},
              Tags).

update(Name, Value, Tags, Streams) ->
  Stream = maps:get(Tags, Streams, #{}),
  Stream2 = maps:put(Name, Value, Stream),
  maps:put(Tags, Stream2, Streams).

to_maybe(undefined) ->
  {nothing};

to_maybe(Value) ->
  {just, Value}.
