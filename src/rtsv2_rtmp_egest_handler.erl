-module(rtsv2_rtmp_egest_handler).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/rtmp.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/stream_sync.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/receive_from_bus_generator.hrl").
-include("./rtsv2_bus_messages.hrl").

-export([
         init/3,
         handle/1
        ]).

-define(state, ?MODULE).

-record(?state,
        {
         rtmp_pid :: pid(),
         workflow :: pid(),
         egest_key,
         slot_lookup,
         play_request,
         profiles,
         on_stream_callback :: fun(),

         slotname
        }).

init(Rtmp, ConnectArgs, [#{ startStream := StartStream, slotLookup := SlotLookup, addClient := AddClient }]) ->
  {_, AppArg} = lists:keyfind("app", 1, ConnectArgs),

  [AppName | _] = string:split(AppArg, "?"),





  % UUID = rtsv2_types:string_to_uuid(unicode:characters_to_binary(AppName,utf8)),
  % EgestKey = {egestKey, UUID, {primary}},

  % StartStream(EgestKey),
  % ?INFO("Got RTMP egest connection, started stream for ~p", [EgestKey]),

  
  % SlotConfig = SlotLookup(EgestKey),
  % ?INFO("Looked up ~p to ~p", [EgestKey, SlotConfig]),
  % State = #?state{rtmp_pid = Rtmp, egest_key = EgestKey, slot_lookup = SlotLookup },
  % State2 = wait_for_slot_config(State),

  % AddClient(self(), EgestKey, <<"fake_session_id">>),

  {ok, #?state{ rtmp_pid = Rtmp, slotname = AppName }}.


handle(State = #?state{ slotname = SlotName }) ->
  %% the workflow is dealing with the RTMP (once it starts up), so just wait until it says we are done
  receive

    PlayRequest = {_Rtmp, {request, play, {_StreamId, _ClientId, _Path}}} ->
      ?INFO("Play request with path ~p, slot name ~p", [_Path, SlotName]),
      % handle(maybe_start(State#?state{play_request = PlayRequest}));
      handle(State);


    #workflow_output{message = #workflow_data_msg{data = disconnected}} ->
      ok;

    {egestCurrentActiveProfiles, _Profiles} -> handle(State);

    slot_lookup ->
      State2 = wait_for_slot_config(State),
      handle(maybe_start(State2));


    Other ->
      ?WARNING("Unexpected workflow output ~p", [Other]),
      handle(State)
  end.

wait_for_slot_config(State = #?state{slot_lookup = SlotLookup, egest_key = EgestKey}) ->
  SlotConfig = SlotLookup(EgestKey),
  ?INFO("Looked up ~p to ~p", [EgestKey, SlotConfig]),
  case SlotConfig of
    {nothing} -> timer:send_after(100, slot_lookup), State;
    {just, #{profiles := Profiles}} ->
      State#?state{ profiles = Profiles }
  end.

maybe_start(State = #?state { rtmp_pid = Rtmp,
                              egest_key = EgestKey,
                              workflow = undefined,
                              play_request = PlayRequest =  {_Rtmp, {request, play, {_StreamId, _ClientId, Path}}},
                              profiles = Profiles }) when is_list(Profiles) ->

  ?INFO("Starting with path ~p and profiles ~p", [Path, Profiles]),
  StreamName = unicode:characters_to_binary(Path, utf8),
  case lists:search(fun (#{profileName := Name}) -> Name == StreamName end, Profiles) of
    false -> 
      ?INFO("Couldn't find profile ~p", [StreamName]),
      State;
    {value, Profile} -> 
      {ok, WorkflowPid } = start_workflow(EgestKey, Rtmp, Profile),
      {ok, Handle} = id3as_workflow:workflow_handle(WorkflowPid),
      {ok, Nodes} = id3as_workflow:get_nodes(Handle),
      case lists:search(fun (#workflow_node{ name = Name }) -> Name =:= rtmp_egest end, Nodes) of
        {value, #workflow_node{ pid = Pid }} ->
          ?INFO("Found egest node at pid ~p", Pid),
          Pid ! PlayRequest
      end,
      State#?state{ workflow = WorkflowPid }
  end;
  
maybe_start(State) -> State.


start_workflow(EgestKey, Rtmp, Profile = #{ firstAudioSSRC := AudioSSRC, firstVideoSSRC := VideoSSRC, profileName := ProfileName }) ->
  ?INFO("Starting workflow for profile ~p", [Profile]),
 %% {video_profile,undefined,h264,{codec_profile_level,66,3.0},false,720,400,undefined,yuv420p,{1,1},{24000,1001},undefined,undefined,undefined,undefined,undefined},{video_frame_metadata,false,false,false,false},undefined,false,false,false,false,-576460483451168,1592408029892,clock,undefined,143316722689200,143316722689200,0,<<0,0,0,1,65,154,160,26,188,8,1,57,191,...>>,[],false}


  VideoMetadata = #rtmp_video_metadata{
                      video_key_frame_frequency = 5,
                      video_data_rate = 0, %% 500
                      avcprofile = 66
                      },


  % KeyFrameFrequency = case FrameRate of
  %                       {Num, Den} ->
  %                         case media_utils:gop_size(VideoProfile) of
  %                           undefined ->
  %                             1;
  %                           GopSize ->
  %                             (GopSize * Den) div Num
  %                         end;
  %                       _ ->
  %                         1
  %                     end,


                    %% set from profile:
                  %  frame_rate = IncomingFrameRate,
                  %   width = IncomingWidth,
                  %   height = IncomingHeight,
                  %   video_codec_id = VideoCodecId
  AudioMetadata = #rtmp_audio_metadata{
                      audio_data_rate = 0,%% 48
                      audio_sample_size = media_utils:sample_format_to_bits(s16)
                      %% audio_sample_rate, audio_channels, audio_codec_id set from profile
                      },
  RtmpConfig = #rtmp_pull_egest_processor_config{
                  rtmp = Rtmp,
                  video_metadata = VideoMetadata,
                  audio_metadata = AudioMetadata
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
