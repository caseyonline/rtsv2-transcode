-module(rtsv2_rtmp_ingest_handler).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_common/include/id3as_types.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/rtmp.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").
-include_lib("id3as_media/include/frame_writer.hrl").
-include_lib("id3as_media/include/bitrate_monitor.hrl").
-include_lib("id3as_avp/include/avp_ingest_source_details_extractor.hrl").

-export([
         init/3,
         handle/1
        ]).

-define(state, ?MODULE).

-record(?state,
        {
         rtmp_pid :: pid(),
         ingestStartedFn :: fun(),
         ingestStoppedFn :: fun(),
         streamAuthTypeFn :: fun(),
         streamAuthFn :: fun(),
         streamPublishFn :: fun(),
         clientMetadataFn :: fun(),
         sourceInfoFn :: fun(),
         streamDetails :: term(),
         streamAndVariant :: term()
        }).

init(Rtmp, ConnectArgs, [#{ ingestStarted := IngestStartedFn
                          , ingestStopped := IngestStoppedFn
                          , streamAuthType := StreamAuthTypeFn
                          , streamAuth := StreamAuthFn
                          , streamPublish := StreamPublishFn
                          , clientMetadata := ClientMetadataFn
                          , sourceInfo := SourceInfoFn }]) ->

  {_, TcUrl} = lists:keyfind("tcUrl", 1, ConnectArgs),

  #rtmp_parsed_url{
     connection_config = #rtmp_connection_config{
                            host = Host
                           },
     app_config = #rtmp_app_config{
                     application = ShortName
                    },
     query = Query
    } = rtmp_utils:parse_url(TcUrl),

  State = #?state{rtmp_pid = Rtmp,
                  ingestStartedFn = IngestStartedFn,
                  ingestStoppedFn = IngestStoppedFn,
                  streamAuthTypeFn = StreamAuthTypeFn,
                  streamAuthFn = StreamAuthFn,
                  clientMetadataFn = ClientMetadataFn,
                  sourceInfoFn = SourceInfoFn
                 },

  case Query of
    #{<<"authmod">> := <<"adobe">>,
      <<"user">> := UserName,
      <<"challenge">> := ClientChallenge,
      <<"response">> := ClientResponse} ->
      %% We have a adobe completed digest

      Reply = (StreamAuthFn(Host, ShortName, UserName))(),

      case Reply of
        {nothing} ->
          ?SLOG_INFO("Abobe StreamAuth rejected", #{host => Host,
                                                    shortName => ShortName,
                                                    username => UserName}),
          {stop, rejected};

        {just, #{username := ExpectedUserName,
                 password := ExpectedPassword}} ->

          case rtmp:compare_adobe_challenge_response(ExpectedUserName,
                                                     <<"NzEwNzk">>,
                                                     ExpectedPassword,
                                                     <<"ODE3MDQ3NTYz">>,
                                                     ClientChallenge,
                                                     ClientResponse) of
            true ->
              {ok, State#?state{streamPublishFn = StreamPublishFn(Host, ShortName, UserName)}};

            false ->
              ?SLOG_INFO("Challenge failed", #{client_challenge => ClientChallenge,
                                               client_response => ClientResponse}),
              {authenticate_fail, adobe, invalid_password}
          end
      end;

    #{<<"authmod">> := <<"adobe">>,
      <<"user">> := UserName} ->
      ?INFO("Username Matches - do next phase"),
      {authenticate, adobe, UserName, <<"NzEwNzk">>, <<"ODE3MDQ3NTYz">>};

    #{<<"authmod">> := <<"llnw">>,
      <<"user">> := UserName,
      <<"nonce">> := ClientOurNonce,
      <<"cnonce">> := ClientNonce,
      <<"nc">> := ClientNc,
      <<"response">> := ClientResponse} ->
      %% We have a llnw completed digest

      Reply = (StreamAuthFn(Host, ShortName, UserName))(),

      case Reply of
        {nothing} ->
          ?SLOG_INFO("LLNW StreamAuth rejected", #{host => Host,
                                                   shortName => ShortName,
                                                   username => UserName}),
          {stop, rejected};

        {just, #{username := ExpectedUserName,
                 password := ExpectedPassword}} ->
          Realm = <<"live">>,
          Method = <<"publish">>,
          Nonce = <<"ODE3MDQ3NTYz">>,
          Qop = <<"auth">>,

          case rtmp:compare_llnw_challenge_response(ExpectedUserName,
                                                    Realm,
                                                    ExpectedPassword,
                                                    Method,
                                                    ShortName,
                                                    Nonce,
                                                    ClientNc,
                                                    ClientNonce,
                                                    Qop,
                                                    ClientResponse) of
            true ->
              {ok, State#?state{streamPublishFn = StreamPublishFn(Host, ShortName, UserName)}};

            false ->
              ?SLOG_INFO("Challenge failed", #{client_response => ClientResponse}),
              {authenticate_fail, adobe, invalid_password}
          end
      end;


    #{<<"authmod">> := <<"llnw">>,
      <<"user">> := UserName} ->
      ?INFO("Username Matches - do next phase"),
      {authenticate, llnw, UserName, <<"ODE3MDQ3NTYz">>};

    #{} ->
      ?SLOG_INFO("No authmod - start authentication", #{host => Host,
                                                        shortName => ShortName}),
      Reply = (StreamAuthTypeFn(Host, ShortName))(),

      case Reply of
        {nothing} ->
          {stop, rejected};

        {just, #{authType := {Protocol}}} ->
          {authenticate, Protocol}
      end
  end.

handle(State = #?state{rtmp_pid = Rtmp,
                       streamPublishFn = StreamPublishFn,
                       ingestStartedFn = IngestStartedFn}) ->

  receive
    {Rtmp, {request, publish, {StreamId, ClientId, Path}}} ->
      [StreamNameStr | _] = string:tokens(Path, "&"),

      StreamName = list_to_binary(StreamNameStr),

      case (StreamPublishFn(StreamName))() of
        {nothing} ->
          ?SLOG_INFO("Streamname rejected", #{stream_name => StreamName}),
          rtmp:close(Rtmp),
          ok;

        {just, StreamDetails} ->
          ?SLOG_INFO("Inbound stream", #{stream_id => StreamId,
                                         client_id => ClientId,
                                         path => Path,
                                         stream_name => StreamName}),

          {ok, {RemoteIp, RemotePort}} = rtmp:peername(Rtmp),

          case (IngestStartedFn(StreamDetails, StreamName, list_to_binary(inet:ntoa(RemoteIp)), RemotePort, self()))() of
            {right, IngestKey} ->
              {ok, WorkflowPid} = start_workflow(Rtmp, StreamId, ClientId, Path, IngestKey),

              %% Stream is now connected - we block in here, when we return the rtmp_server instance will close
              workflow_loop(StreamName, WorkflowPid, State#?state{streamDetails = StreamDetails,
                                                                  streamAndVariant = IngestKey});

            {left, _} ->
              ?SLOG_INFO("Invalid stream name"),
              rtmp:close(Rtmp),
              ok
          end
      end;

    {Rtmp, disconnected} ->
      ok
  end.

workflow_loop(StreamName, WorkflowPid, State = #?state{ingestStoppedFn = IngestStoppedFn,
                                                       streamDetails = StreamDetails,
                                                       clientMetadataFn = ClientMetadataFn,
                                                       sourceInfoFn = SourceInfoFn,
                                                       streamAndVariant = IngestKey}) ->
  %% the workflow is dealing with the RTMP, so just wait until it says we are done
  receive
    #workflow_output{message = #no_active_generators_msg{}} ->
      ?SLOG_WARNING("Client exited"),

      unit = (IngestStoppedFn(StreamDetails, StreamName))(),
      ok;

    #workflow_output{message = #workflow_data_msg{data = #rtmp_client_metadata{metadata = Metadata}}} ->
      unit = (ClientMetadataFn(IngestKey, Metadata))(),
      workflow_loop(StreamName, WorkflowPid, State);

    #workflow_output{message = #workflow_data_msg{data = SourceInfo = #source_info{}}} ->
      unit = (SourceInfoFn(IngestKey, SourceInfo))(),
      workflow_loop(StreamName, WorkflowPid, State);

    Other ->
      ?SLOG_WARNING("Unexpected workflow output", #{output => Other}),
      workflow_loop(StreamName, WorkflowPid, State)
  end.

start_workflow(Rtmp, StreamId, ClientId, Path, Key = {ingestKey, SlotName, ProfileName, StreamRole}) ->

  Workflow = #workflow{
                name = {rtmp_ingest_handler, Key},
                display_name = <<"RTMP Ingest">>,
                tags = #{type => rtmp_ingest_handler,
                         slot => SlotName,
                         profile => ProfileName,
                         stream_role => StreamRole
                        },
                generators = [
                              #generator{name = rtmp_ingest,
                                         module = rtmp_push_ingest_generator,
                                         config = #rtmp_push_ingest_generator_config{
                                                     rtmp = Rtmp,
                                                     auto_accept = {StreamId, ClientId, Path}
                                                    }}
                             ],
                processors = [
                              #processor{name = demux,
                                         subscribes_to = rtmp_ingest,
                                         module = rtmp_tag_to_frame
                                        },

                              #processor{name = set_source_id,
                                         subscribes_to = ?previous,
                                         module = set_source_id,
                                         config = {ProfileName, make_ref()}
                                        },

                              %% #processor{name = reorder_slices,
                              %%            subscribes_to = ?previous,
                              %%            module = h264_reorder_slices
                              %%           },

                              #processor{name = program_details,
                                         subscribes_to = ?previous,
                                         module = program_details_generator
                                        },

                              %% Updates frame to add estimated bitrate,
                              %% and outputs #bitrate_info messages
                              #processor{name = source_bitrate_monitor,
                                         display_name = <<"Source Bitrate Monitor">>,
                                         subscribes_to = ?previous,
                                         module = stream_bitrate_monitor,
                                         config = #bitrate_monitor_config{
                                                     default_profile_name = source,
                                                     mode = passthrough_with_update,
                                                     output_bitrate_info_messages = false
                                                    }
                                        },

                              #processor{name = send_to_bus,
                                         subscribes_to = {?previous, ?frames},
                                         module = send_to_bus_processor,
                                         config = #send_to_bus_processor_config{consumes = true,
                                                                                bus_name = {ingest, IngestKey}}
                                        },

                              %%--------------------------------------------------
                              %% Reporting
                              %%--------------------------------------------------

                              %% Count inbound frames - no output, just meters
                              #processor{name = source_frame_meter,
                                         display_name = <<"Source Frame Meter">>,
                                         subscribes_to = source_bitrate_monitor,
                                         module = frame_flow_meter},

                              %% Extracts source details (frame rates etc) for reporting purposes
                              %% Generates #source_info{} records
                              #processor{name = source_details_extractor,
                                         display_name = <<"Source Details Extractor">>,
                                         subscribes_to = source_bitrate_monitor,
                                         module = source_details_extractor},

                              %% #processor{name = writer,
                              %%            subscribes_to = {reorder_slices, ?video_frames},
                              %%            module = frame_writer,
                              %%            config = #frame_writer_config{filename = "/tmp/out.h264", mode = consumes}
                              %%           }

                              []
                             ]
               },

  {ok, WorkflowPid} = id3as_workflow:start_link(Workflow),

  {ok, WorkflowPid}.
