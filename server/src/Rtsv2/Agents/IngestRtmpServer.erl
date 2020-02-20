-module(rtsv2_agents_ingestRtmpServer@foreign).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/rtmp.hrl").
-include_lib("id3as_media/include/bitrate_monitor.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").
-include_lib("id3as_avp/include/avp_ingest_source_details_extractor.hrl").

-export([ startServerImpl/6
        , rtmpQueryToPurs/1
        , compareAdobeChallengeImpl/6
        , compareLlnwChallengeImpl/7
        , startWorkflowImpl/5
        ]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

startServerImpl(Left, Right, {ipv4, O1, O2, O3, O4}, Port, NbAcceptors, Callbacks) ->
  fun() ->
      case rtmp_server:start_listener({rtmp_listener, Port},
                                      NbAcceptors,
                                      [{ip, {O1, O2, O3, O4}}, {port, Port}],
                                      [{dispatch, [{'*', rtsv2_rtmp_ingest_handler, [Callbacks]}]},
                                       {config, #rtmp_server_config{}}]) of
        {ok, _} -> Right;
        {error, {alread_started, _}} -> Right;
        Error -> Left(Error)
      end
  end.

startWorkflowImpl(Rtmp, PublishArgs, IngestKey, ClientMetadataFn, SourceInfoFn) ->
  fun() ->
      {ok, _WorkflowPid} = start_workflow(Rtmp, PublishArgs, IngestKey),

      %% Stream is now connected - we block in here, when we return the rtmp_server instance will close
      workflow_loop(ClientMetadataFn, SourceInfoFn)
  end.

rtmpQueryToPurs(#{<<"authmod">> := <<"adobe">>,
                  <<"user">> := UserName,
                  <<"challenge">> := ClientChallenge,
                  <<"response">> := ClientResponse}) ->
  {adobePhase2, #{ username => UserName
                 , clientChallenge => ClientChallenge
                 , clientResponse => ClientResponse
                 }};

rtmpQueryToPurs(#{<<"authmod">> := <<"adobe">>,
                  <<"user">> := UserName}) ->
  {adobePhase1, #{ username => UserName
                 }};

rtmpQueryToPurs(#{<<"authmod">> := <<"llnw">>,
                  <<"user">> := UserName,
                  <<"nonce">> := ClientOurNonce,
                  <<"cnonce">> := ClientNonce,
                  <<"nc">> := ClientNc,
                  <<"response">> := ClientResponse}) ->
  {llnwPhase2, #{ username => UserName
                , ourNonce => ClientOurNonce
                , clientNonce => ClientNonce
                , clientNc => ClientNc
                , clientResponse => ClientResponse
                }};

rtmpQueryToPurs(#{<<"authmod">> := <<"llnw">>,
                  <<"user">> := UserName}) ->
  {llnwPhase1, #{ username => UserName
                }};

rtmpQueryToPurs(#{}) ->
  {initial}.

compareAdobeChallengeImpl(Username, Salt, Password, Challenge, ClientChallenge, ClientResponse) ->
  rtmp:compare_adobe_challenge_response(Username, Salt, Password, Challenge, ClientChallenge, ClientResponse).

compareLlnwChallengeImpl(Username, Password, ShortName, Nonce, ClientNc, ClientNonce, ClientResponse) ->

  Realm = <<"live">>,
  Method = <<"publish">>,
  Nonce = <<"ODE3MDQ3NTYz">>,
  Qop = <<"auth">>,

  rtmp:compare_llnw_challenge_response(Username,
                                       Realm,
                                       Password,
                                       Method,
                                       ShortName,
                                       Nonce,
                                       ClientNc,
                                       ClientNonce,
                                       Qop,
                                       ClientResponse).

%%------------------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------------------
start_workflow(Rtmp, PublishArgs, Key = {ingestKey, SlotName, StreamRole, ProfileName}) ->

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
                                                     auto_accept = PublishArgs
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
                                                                                bus_name = {ingest, Key}}
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

workflow_loop(ClientMetadataFn, SourceInfoFn) ->
  %% the workflow is dealing with the RTMP, so just wait until it says we are done
  receive
    #workflow_output{message = #no_active_generators_msg{}} ->
      ?SLOG_WARNING("Client exited"),
      ok;

    #workflow_output{message = #workflow_data_msg{data = #rtmp_client_metadata{metadata = Metadata}}} ->
      unit = (ClientMetadataFn(Metadata))(),
      workflow_loop(ClientMetadataFn, SourceInfoFn);

    #workflow_output{message = #workflow_data_msg{data = SourceInfo = #source_info{}}} ->
      unit = (SourceInfoFn(SourceInfo))(),
      workflow_loop(ClientMetadataFn, SourceInfoFn);

    Other ->
      ?SLOG_WARNING("Unexpected workflow output", #{output => Other}),
      workflow_loop(ClientMetadataFn, SourceInfoFn)
  end.
