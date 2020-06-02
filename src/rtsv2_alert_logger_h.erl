-module(rtsv2_alert_logger_h).

-include_lib("id3as_common/include/id3as_message_bus.hrl").

-export([ log/2
        ]).

log(LogEvent, Config = #{config := #{message_bus := Bus}}) ->
  try
    ?I_RAISE_BUS_MSG(Bus, LogEvent)
  catch
    Class:Reason ->
      io:format(user, "CRASH ~p / ~p~n", [Class, Reason]),
      ok
  end,
  ok.

%% Purescript logging
%% GOT:#{f =>
%%           #{level => info,
%%             meta =>
%%                 #{domain => ['IngestAggregator','Instance'],
%%                   file =>
%%                       "/Users/steve/dev/rtsv2/server/src/Rtsv2/Agents/IngestAggregatorInstance.purs",
%%                   gl => <0.213.0>,line => 936,
%%                   mfa =>
%%                       {'Rtsv2.Agents.IngestAggregatorInstance@ps',
%%                           '-doAddLocalIngest/3-fun-2-',8},
%%                   misc =>
%%                       #{profileName => <<"high">>,
%%                         slotId => <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>,
%%                         slotRole => {primary}},
%%                   module => 'Rtsv2.Agents.IngestAggregatorInstance@ps',
%%                   pid => <0.395.0>,time => 1591023093927476},
%%             msg => {"Local Ingest added",[]}}}

%% Non-structured id3as-media logging
%% GOT:#{f =>
%%           #{level => debug,
%%             meta =>
%%                 #{file =>
%%                       "/Users/steve/dev/rtsv2/_build/default/lib/id3as_media/src/workflow/workflow_processor.erl",
%%                   gl => <0.213.0>,line => 118,
%%                   mfa => {workflow_processor,init,1},
%%                   pid => <0.454.0>,time => 1591023094534447,
%%                   workflow =>
%%                       #{node_name => audio_transcode_opus_and_aac_decode,
%%                         path =>
%%                             [audio_transcode_opus_and_aac_decode,
%%                              audio_transcode,audio_transcode,high,high,
%%                              {ingest_aggregator_instance,
%%                                  <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>}],
%%                         root_workflow_name =>
%%                             {ingest_aggregator_instance,
%%                                 <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>},
%%                         type => processor}},
%%             msg =>
%%                 {"Processor '~s' started on pid ~p (~p)",
%%                  [audio_transcode_opus_and_aac_decode,<0.454.0>,
%%                   libav_audio_decoder]}}}

%% Structure logging from erlang
%% GOT:#{f =>
%%           #{level => info,
%%             meta =>
%%                 #{file =>
%%                       "/Users/steve/dev/rtsv2/src/rtsv2_ingest_aggregator_processor.erl",
%%                   gl => <0.213.0>,line => 108,
%%                   mfa => {rtsv2_ingest_aggregator_processor,process_input,2},
%%                   misc => #{delta => 143192078498610,stream => <<"high">>},
%%                   pid => <0.437.0>,time => 1591023094450826,type => info,
%%                   workflow =>
%%                       #{node_name => aggregate,
%%                         path =>
%%                             [aggregate,
%%                              {ingest_aggregator_instance,
%%                                  <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>}],
%%                         root_workflow_name =>
%%                             {ingest_aggregator_instance,
%%                                 <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>},
%%                         type => processor}},
%%             msg => {string,"Stream selected as reference"}}}
