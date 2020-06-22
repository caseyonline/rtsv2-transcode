-module(rtsv2_rtmp_egest_handler).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/rtmp.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/stream_sync.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/receive_from_bus_generator.hrl").
-include("./rtsv2_bus_messages.hrl").
-include("./rtsv2_types.hrl").

-define(STATS_INTERVAL_MS, 1000).

-export([
         init/3,
         handle/1
        ]).

-record(rtmp_egest_initial_state, {
  rtmp_pid :: pid(),
  slot_id :: slot_id(),
  callbacks :: maps:map()
}).

-record(rtmp_egest_starting_state, {
  rtmp_pid :: pid(),
  play_request :: term(),
  callbacks :: maps:map(),
  egest_key :: term(),
  stream_name :: binary(),
  session_id :: binary()
}).

-record(rtmp_egest_state, {
  rtmp_pid :: pid(),
  workflow :: pid(),
  stats_timer :: timer:tref(),
  callbacks :: maps:map(),
  egest_key :: term(),
  session_id :: binary()
}).

init(Rtmp, ConnectArgs, [Callbacks]) ->
  {_, AppArg} = lists:keyfind("app", 1, ConnectArgs),
  [AppName | _] = string:split(AppArg, "?"),
  UUID = rtsv2_types:string_to_uuid(unicode:characters_to_binary(AppName,utf8)),

  {ok, #rtmp_egest_initial_state{ rtmp_pid = Rtmp, slot_id = UUID, callbacks = Callbacks }}.


handle(State = #rtmp_egest_initial_state{ rtmp_pid = Rtmp, slot_id = SlotId, callbacks = Callbacks = #{ addClient := AddClient, startStream := StartStream, slotLookup := SlotLookup } }) ->
  receive
    PlayRequest = {_Rtmp, {request, play, {_StreamId, _ClientId, Path}}} ->
      [StreamName | Query ] = string:split(Path, "?"),
      Qs = case Query of
            [QueryString] -> 
              maps:from_list(rtmp_utils:parse_qs(list_to_binary(QueryString)));
            _ -> #{}
           end,
      Role = case maps:get(<<"role">>, Qs, undefined) of
        <<"primary">> -> {primary};
        <<"backup">> -> {backup};
        undefined -> {primary}
      end,

      EgestKey = {egestKey, SlotId, Role},
      StartStream(EgestKey),
      ?INFO("Got RTMP egest connection, started stream for ~p", [EgestKey]),

      SlotConfig = SlotLookup(EgestKey),
      ?INFO("Looked up ~p to ~p", [EgestKey, SlotConfig]),

      %% TODO see rtsv2_player_ws_resource      
      ServerId = 1,
      TraceId = format_trace_id(generate_trace_id(ServerId)),

      SessionId = <<"rtmp.", TraceId/binary>>,
      
      AddClient(self(), EgestKey, SessionId), 

      State2 = #rtmp_egest_starting_state{ rtmp_pid = Rtmp,
                                           callbacks = Callbacks,
                                           egest_key = EgestKey,
                                           play_request = PlayRequest,
                                           stream_name = unicode:characters_to_binary(StreamName, utf8),
                                           session_id = SessionId
                                         },

      case wait_for_slot_config(SlotLookup, EgestKey) of
        not_ready -> handle(State2);
        { ready, Profiles } -> handle(start_workflow(State2, Profiles))
      end;

    Other ->
      ?WARNING("Unexpected message ~p", [Other]),
      handle(State)
  end;

handle(State = #rtmp_egest_starting_state{ callbacks = #{ slotLookup := SlotLookup }, egest_key = EgestKey }) ->
  receive
    slot_lookup ->
      case wait_for_slot_config(SlotLookup, EgestKey) of
        not_ready -> handle(State);
        { ready, Profiles } -> handle(start_workflow(State, Profiles))
      end;

    Other ->
      ?WARNING("Unexpected message ~p", [Other]),
      handle(State)
  end;

handle(State = #rtmp_egest_state{ stats_timer = Timer }) ->
  %% the workflow is dealing with the RTMP, so just wait until it says we are done
  receive
    #workflow_output{message = #workflow_data_msg{data = disconnected}} ->
      timer:cancel(Timer),
      send_stats(State),
      ok;

    update_stats ->
      send_stats(State),
      handle(State);

    {egestCurrentActiveProfiles, _Profiles} ->
      handle(State);

    Other ->
      ?WARNING("Unexpected workflow output ~p", [Other]),
      handle(State)
  end.

generate_trace_id(ThisServerId) ->
  << LocallyUniqueId:48/big-integer >> = crypto:strong_rand_bytes(6),
  { ThisServerId, LocallyUniqueId }.


format_trace_id({ServerId, ServerScopedClientId}) ->
  <<(integer_to_binary(ServerId))/binary, ".",
    (integer_to_binary(ServerScopedClientId))/binary
  >>.


wait_for_slot_config(SlotLookup, EgestKey) ->
  SlotConfig = SlotLookup(EgestKey),
  ?INFO("Looked up ~p to ~p", [EgestKey, SlotConfig]),
  case SlotConfig of
    {nothing} ->
      timer:send_after(100, slot_lookup),
      not_ready;
    {just, #{profiles := Profiles}} ->
      {ready, Profiles }
  end.

send_stats(#rtmp_egest_state{ rtmp_pid = Rtmp, egest_key = EgestKey, session_id = SessionId, callbacks = #{ statsUpdate := StatsUpdate }}) ->
  {Sent, Received} = rtmp:get_stats(Rtmp),
  StatsUpdate(EgestKey, #{sessionId => SessionId, octetsSent => Sent, octetsReceived => Received}).

start_workflow(#rtmp_egest_starting_state{ rtmp_pid = Rtmp,
                                           egest_key = EgestKey,
                                           play_request = {_Rtmp, {request, play, PlayRequest}},
                                           stream_name = StreamName,
                                           callbacks = Callbacks,
                                           session_id = SessionId
                                         },
                                          Profiles) ->
  case lists:search(fun (#{profileName := Name}) -> Name == StreamName end, Profiles) of
    false -> 
      ?INFO("Couldn't find profile ~p", [StreamName]),
      throw({no_profile, StreamName});
    {value, Profile} -> 
      {ok, WorkflowPid } = start_workflow(EgestKey, Rtmp, PlayRequest, Profile),
      {ok, Timer} = timer:send_interval(?STATS_INTERVAL_MS, update_stats),
      #rtmp_egest_state{ workflow = WorkflowPid, stats_timer = Timer, rtmp_pid = Rtmp, egest_key = EgestKey, session_id = SessionId, callbacks = Callbacks }
  end.


start_workflow(EgestKey, Rtmp, PlayRequest,
               _Profile = #{ profileName := ProfileName }) ->
  VideoMetadata = #rtmp_video_metadata{
                      video_key_frame_frequency = 5,
                      video_data_rate = 0, %% 500
                      avcprofile = 66
                  },
  AudioMetadata = #rtmp_audio_metadata{
                      audio_data_rate = 0,%% 48
                      audio_sample_size = media_utils:sample_format_to_bits(s16)
                      },
  RtmpConfig = #rtmp_pull_egest_processor_config{
                  rtmp = Rtmp,
                  video_metadata = VideoMetadata,
                  audio_metadata = AudioMetadata,
                  auto_play = PlayRequest
                 },
  ?INFO("Starting rtmp egest from bus ~p", [?RTMP_EGEST_BUS(EgestKey, ProfileName)]),
  Workflow = #workflow{
                name = egest,
                generators = [
                              #generator{name = source,
                                         display_name = <<"Receive from bus">>,
                                         module = receive_from_bus_generator,
                                         config = #receive_from_bus_generator_config{ bus_name = ?RTMP_EGEST_BUS(EgestKey, ProfileName) }
                                        }
                             ],
                processors = [
                              #processor{name = rtmp_egest,
                                         subscribes_to = source,
                                         module = rtmp_pull_egest_processor,
                                         config = RtmpConfig}
                             ]
               },

  {ok, WorkflowPid} = id3as_workflow:start_link(Workflow),

  {ok, WorkflowPid}.
