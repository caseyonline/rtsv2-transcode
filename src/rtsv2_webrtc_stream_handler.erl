-module(rtsv2_webrtc_stream_handler).

-behavior(webrtc_stream_handler).

-export([ init/1
        , handle_info/2
        ]).


-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_rtc/include/rtp.hrl").

-define(state, ?MODULE).

-record(?state,
        {
         stats_gatherer :: atom()
        }).

init(_Args = [StreamAndVariant]) -> %% , StatsGatherer]) ->

  gproc:reg({p, l, {webrtc_stream_output, StreamAndVariant}}),

  timer:send_interval(1000, stats),

  #?state{}. %%stats_gatherer = StatsGatherer}.

handle_info(#rtp_sequence{} = Msg, State) ->
  { broadcast, Msg, State };

handle_info(stats, State) ->
  { request_statistics, State};

handle_info(#frame{}, State) ->
  { noreply, State};

handle_info({statistics, _Stats}, State = #?state{stats_gatherer = undefined}) ->
  {noreply, State};

handle_info({statistics, Stats}, State = #?state{stats_gatherer = StatsGatherer}) ->

  #{server_id := Id} = Stats,

  Pid = gproc:whereis_name({n, l, {webrtc_stream_server, Id}}),
  [{message_queue_len, Len}] = erlang:process_info(Pid, [message_queue_len]),

  StatsGatherer ! {webrtc_stats, Id, maps:put(message_queue_len, Len, Stats)},

  { noreply, State}.
