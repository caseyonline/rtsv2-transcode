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

%% Purescript logging:
%%           #{level => info,
%%             meta =>
%%                 #{domain => ['IntraPoP'],
%%                   file =>
%%                       "/Users/steve/dev/rtsv2/server/src/Rtsv2/Agents/IntraPoP.purs",
%%                   gl => <0.213.0>,line => 1169,
%%                   mfa =>
%%                       {'Rtsv2.Agents.IntraPoP@ps','-membersAlive/2-fun-8-',2},
%%                   module => 'Rtsv2.Agents.IntraPoP@ps',pid => <0.220.0>,
%%                   text => "Members Alive",time => 1591091915484887},
%%             msg =>
%%                 {report,#{members =>
%%                               [<<"172.16.171.2">>,<<"172.16.171.1">>]}}}

%% Erlang structured:
%%           #{level => info,
%%             meta =>
%%                 #{file =>
%%                       "/Users/steve/dev/rtsv2/src/rtsv2_ingest_aggregator_processor.erl",
%%                   gl => <0.213.0>,line => 108,
%%                   mfa => {rtsv2_ingest_aggregator_processor,process_input,2},
%%                   pid => <0.382.0>,text => "Stream selected as reference",
%%                   time => 1591092008109512,type => trace,
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
%%             msg => {report,#{delta => 143198280727920,stream => <<"high">>}}}}

%% Erlang non-structured:
%%           #{level => info,
%%             meta =>
%%                 #{file =>
%%                       "/Users/steve/dev/rtsv2/_build/default/lib/id3as_media/src/drivers/audio_transcode.erl",
%%                   gl => <0.213.0>,line => 221,
%%                   mfa => {audio_transcode,build_processor,3},
%%                   pid => <0.368.0>,time => 1591092008119806,
%%                   workflow =>
%%                       #{node_name => audio_transcode,
%%                         path =>
%%                             [audio_transcode,high,high,
%%                              {ingest_aggregator_instance,
%%                                  <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>}],
%%                         root_workflow_name =>
%%                             {ingest_aggregator_instance,
%%                                 <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>},
%%                         type => processor}},
%%             msg =>
%%                 {"Filter graph ~p (in: ~p, out: ~p)",
%%                  [<<"aformat=sample_fmts=s16:sample_rates=48000:channel_layouts=stereo">>,
%%                   {audio_profile,undefined,raw,
%%                       {codec_profile_level,4,0},
%%                       48000,fltp,stereo,undefined,undefined,undefined},
%%                   {audio_profile,undefined,raw,
%%                       {codec_profile_level,4,0},
%%                       48000,s16,stereo,undefined,undefined,undefined}]}}}
