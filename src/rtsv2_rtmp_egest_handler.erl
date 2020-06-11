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
         on_stream_callback :: fun()
        }).

init(Rtmp, ConnectArgs, [#{ startStream := StartStream  }]) ->
  %% big **ing TODO
  EgestKey = {egestKey, << 1:128/big-unsigned-integer >>, {primary}},

  {_, AppArg} = lists:keyfind("app", 1, ConnectArgs),

  [ShortName | Rem] = string:split(AppArg, "?"),

  Query = case Rem of
            [] -> #{};
            [QueryString] -> maps:from_list(rtmp_utils:parse_qs(list_to_binary(QueryString)))
          end,

  ?NOTICE("Hey I got an rtmp egest connection!"),

  StartStream(EgestKey),
  ?INFO("Started stream for ~p", [EgestKey]),

  {ok, WorkflowPid } = start_workflow(EgestKey, Rtmp),

  ?INFO("Started workflow for ~p", [EgestKey]),
  {ok, #?state{rtmp_pid = Rtmp, workflow = WorkflowPid}}.


handle(State = #?state{rtmp_pid = Rtmp,
               on_stream_callback = OnStreamCallback}) ->

  {ok, {RemoteIp, RemotePort}} = rtmp:peername(Rtmp),
  RemoteIpStr = list_to_binary(inet:ntoa(RemoteIp)),

  %% the workflow is dealing with the RTMP, so just wait until it says we are done
  receive
    #workflow_output{message = #workflow_data_msg{data = disconnected}} ->
      % multi_port_rtmp_server:remove_client(Rtmp, something),
      ok;

    Other ->
      ?WARNING("Unexpected workflow output ~p", [Other]),
      handle(State)
  end.




start_workflow(EgestKey, Rtmp) ->
  VideoMetadata = #rtmp_video_metadata{
                      video_key_frame_frequency = 5,
                      video_data_rate = 500,
                      avcprofile = 66
                      },
  AudioMetadata = #rtmp_audio_metadata{
                      audio_data_rate = 48
                      },
  RtmpConfig = #rtmp_pull_egest_processor_config{
                  rtmp = Rtmp,
                  video_metadata = VideoMetadata,
                  audio_metadata = AudioMetadata
                 },
  ?INFO("Starting rtmp egest from bus ~p", [{egest_rtmp_bus, EgestKey}]),
  Workflow = #workflow{
                name = egest,
                generators = [
                              #generator{name = source,
                                         display_name = <<"Receive from bus">>,
                                         module = receive_from_bus_generator,
                                         config = #receive_from_bus_generator_config{ bus_name = ?RTMP_EGEST_BUS(EgestKey) }
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
