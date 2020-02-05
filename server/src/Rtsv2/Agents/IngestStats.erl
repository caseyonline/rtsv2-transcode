-module(rtsv2_agents_ingestStats@foreign).

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

      lists:map(fun({Ref, StreamAndVariant}) ->
                    MetricsSpec = [{#workflow_node_status{id =
                                                            #workflow_node_id{ref = Ref,
                                                                              path = ['$1', {rtmp_ingest_handler, {streamAndVariant,'_','_'}}]},
                                                          status = '$2',
                                                          _ = '_'},
                                    [{'orelse',
                                      {'==', '$1', source_bitrate_monitor},
                                      {'==', '$1', source_frame_meter}}],
                                    ['$2']}],

                    Metrics = ets:select(id3as_workflow_node_metrics, MetricsSpec),

                    Extracted = lists:map(fun(#status{module = frame_flow_meter,
                                                      metrics = FrameFlowMetrics}) ->
                                              #{source => <<"frame_flow_meter">>,
                                                metrics => metrics_to_purs(FrameFlowMetrics)};
                                             (#status{module = stream_bitrate_monitor,
                                                      metrics = BitrateMetrics}) ->
                                              #{source => <<"stream_bitrate_monitor">>,
                                                metrics => metrics_to_purs(BitrateMetrics)}
                                          end,
                                          Metrics),

                    #{streamAndVariant => StreamAndVariant,
                       metrics => Extracted}
                end,
                Refs)
  end.

metrics_to_purs(Metrics) ->
  [metric_to_purs(Metric) || Metric <- Metrics].

metric_to_purs(#counter_metric{name = Name,
                               display_name = DisplayName,
                               value = Value,
                               tags = Tags}) ->
  {counter, #{name => Name,
              displayName => DisplayName,
              value => value_to_purs(Value),
              tags => [tag_to_purs(Tag) || Tag <- Tags]
             }};

metric_to_purs(#gauge_metric{name = Name,
                             display_name = DisplayName,
                             value = Value,
                             tags = Tags}) ->
  {gauge, #{name => Name,
            displayName => DisplayName,
            value => value_to_purs(Value),
            tags => [tag_to_purs(Tag) || Tag <- Tags]
           }};

metric_to_purs(#text_metric{name = Name,
                            display_name = DisplayName,
                            value = Value,
                            tags = Tags}) ->

  {text, #{name => Name,
           displayName => DisplayName,
           value => value_to_purs(Value),
           tags => [tag_to_purs(Tag) || Tag <- Tags]
           }}.

tag_to_purs({Name, Value}) ->
  #{name => to_binary(Name),
    value => to_binary(Value)}.

value_to_purs(Value) when is_integer(Value) ->
  {metricInt, Value};

value_to_purs(Value) when is_float(Value) ->
  {metricFloat, Value};

value_to_purs(Value) when is_binary(Value) ->
  {metricString, Value};

value_to_purs(Value) when is_atom(Value) ->
  {metricString, atom_to_binary(Value, utf8)}.

to_binary(Bin) when is_binary(Bin) ->
  Bin;

to_binary(Atom) when is_atom(Atom) ->
  atom_to_binary(Atom, utf8).
