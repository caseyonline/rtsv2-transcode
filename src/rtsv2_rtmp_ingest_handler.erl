-module(rtsv2_rtmp_ingest_handler).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_common/include/id3as_types.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/rtmp.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").
-include_lib("id3as_media/include/frame_writer.hrl").
-include_lib("id3as_media/include/bitrate_monitor.hrl").

-export([
         init/3,
         handle/1
        ]).

-define(state, ?MODULE).

-record(?state,
        {
         rtmp_pid :: pid(),
         ingestStarted :: fun(),
         ingestStopped :: fun(),
         streamAuthType :: fun(),
         streamAuth :: fun(),
         streamPublish :: fun(),
         clientMetadata :: fun(),
         streamDetails :: term(),
         streamAndVariant :: term()
        }).

init(Rtmp, ConnectArgs, [#{ ingestStarted := IngestStarted
                          , ingestStopped := IngestStopped
                          , streamAuthType := StreamAuthType
                          , streamAuth := StreamAuth
                          , streamPublish := StreamPublish
                          , clientMetadata := ClientMetadata }]) ->

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

  case Query of
    #{<<"authmod">> := <<"adobe">>,
      <<"user">> := UserName,
      <<"challenge">> := ClientChallenge,
      <<"response">> := ClientResponse} ->
      %% We have a adobe completed digest

      Reply = (((StreamAuth(Host))(ShortName))(UserName))(),

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
              {ok, #?state{rtmp_pid = Rtmp,
                           ingestStarted = IngestStarted,
                           ingestStopped = IngestStopped,
                           streamAuthType = StreamAuthType,
                           streamAuth = StreamAuth,
                           streamPublish = (((StreamPublish)(Host))(ShortName))(UserName),
                           clientMetadata = ClientMetadata
                          }};

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

      Reply = (((StreamAuth(Host))(ShortName))(UserName))(),

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
              {ok, #?state{rtmp_pid = Rtmp,
                           ingestStarted = IngestStarted,
                           ingestStopped = IngestStopped,
                           streamAuthType = StreamAuthType,
                           streamAuth = StreamAuth,
                           streamPublish = (((StreamPublish)(Host))(ShortName))(UserName),
                           clientMetadata = ClientMetadata
                          }};

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
      Reply = ((StreamAuthType(Host))(ShortName))(),

      case Reply of
        {nothing} ->
          {stop, rejected};

        {just, #{authType := {Protocol}}} ->
          {authenticate, Protocol}
      end
  end.

handle(State = #?state{rtmp_pid = Rtmp,
                       streamPublish = StreamPublish,
                       ingestStarted = IngestStarted}) ->

  receive
    {Rtmp, {request, publish, {StreamId, ClientId, Path}}} ->
      [StreamNameStr | _] = string:tokens(Path, "&"),

      StreamName = list_to_binary(StreamNameStr),

      case ((StreamPublish)(StreamName))() of
        {nothing} ->
          ?SLOG_INFO("Streamname rejected", #{stream_name => StreamName}),
          rtmp:close(Rtmp),
          ok;

        {just, StreamDetails} ->
          ?SLOG_INFO("Inbound stream", #{stream_id => StreamId,
                                         client_id => ClientId,
                                         path => Path,
                                         stream_name => StreamName}),

          case (((IngestStarted(StreamDetails))(StreamName))(self()))() of
            {right, StreamAndVariant} ->
              {ok, WorkflowPid} = start_workflow(Rtmp, StreamId, ClientId, Path, StreamAndVariant),

              %% Stream is now connected - we block in here, when we return the rtmp_server instance will close
              workflow_loop(StreamName, WorkflowPid, State#?state{streamDetails = StreamDetails,
                                                                  streamAndVariant = StreamAndVariant});

            {left, _} ->
              ?SLOG_INFO("Invalid stream name"),
              rtmp:close(Rtmp),
              ok
          end
      end;

    {Rtmp, disconnected} ->
      ok
  end.

workflow_loop(StreamName, WorkflowPid, State = #?state{ingestStopped = IngestStopped,
                                                       streamDetails = StreamDetails,
                                                       clientMetadata = ClientMetadata,
                                                       streamAndVariant = StreamAndVariant}) ->
  %% the workflow is dealing with the RTMP, so just wait until it says we are done
  receive
    #workflow_output{message = #no_active_generators_msg{}} ->
      ?SLOG_WARNING("Client exited"),

      unit = ((IngestStopped(StreamDetails))(StreamName))(),
      ok;

    #workflow_output{message = #workflow_data_msg{data = #rtmp_client_metadata{metadata = Metadata}}} ->
      PursMetadata = rtmp_metadata_to_purs(Metadata),
      unit = ((ClientMetadata(StreamAndVariant))(PursMetadata))(),
      ?SLOG_DEBUG("Got client metadata", #{purs => PursMetadata}),

      workflow_loop(StreamName, WorkflowPid, State);

    Other ->
      ?SLOG_WARNING("Unexpected workflow output", #{output => Other}),
      workflow_loop(StreamName, WorkflowPid, State)
  end.

start_workflow(Rtmp, StreamId, ClientId, Path, StreamAndVariant = {streamAndVariant, SlotName, ProfileName}) ->

  Workflow = #workflow{
                name = {rtmp_ingest_handler, StreamAndVariant},
                display_name = <<"RTMP Ingest">>,
                tags = #{type => rtmp_ingest_handler,
                         slot => SlotName,
                         profile => ProfileName},
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
                                         config = {StreamAndVariant, make_ref()}
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
                                                                                bus_name = {ingest, StreamAndVariant}}
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

rtmp_metadata_to_purs(Metadata) ->
  array:from_list(rtmp_metadata_to_purs_(Metadata)).

rtmp_metadata_to_purs_([]) ->
  [];

rtmp_metadata_to_purs_([{Name, Value} | T]) ->
  [#{name => Name,
     value => rtmp_metadata_value_to_purs(Value)} | rtmp_metadata_to_purs_(T)].

rtmp_metadata_value_to_purs(Value) when Value == true;
                                       Value == false ->
  {rtmpBool, Value};

rtmp_metadata_value_to_purs(Value) when is_integer(Value) ->
  {rtmpInt, Value};

rtmp_metadata_value_to_purs(Value) when is_float(Value) ->
  {rtmpFloat, Value};

rtmp_metadata_value_to_purs(Value) when is_binary(Value) ->
  {rtmpString, Value}.
