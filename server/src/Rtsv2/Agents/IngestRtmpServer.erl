-module(rtsv2_agents_ingestRtmpServer@foreign).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/rtmp.hrl").
-include_lib("id3as_media/include/bitrate_monitor.hrl").
-include_lib("id3as_media/include/send_to_bus_processor.hrl").
-include_lib("id3as_avp/include/avp_ingest_source_details_extractor.hrl").

-export([ startServerImpl/6
        , rtmpQueryToPurs/1
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
                , clientOurNonce => ClientOurNonce
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

%%------------------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------------------
start_workflow(Rtmp, PublishArgs, Key = {ingestKey, SlotId, SlotRole, ProfileName}) ->

  Workflow = #workflow{
                name = {rtmp_ingest_handler, Key},
                display_name = <<"RTMP Ingest">>,
                tags = #{type => rtmp_ingest_handler,
                         slot => SlotId,
                         profile => ProfileName,
                         stream_role => SlotRole
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

                              #processor{name = onfi_to_frame,
                                         subscribes_to = [?previous, {rtmp_ingest, ?rtmp_onfi_messages}],
                                         module = rtmp_onfi_to_frame
                                        },

                              rtsv2_ingest_processor:ingest_processor(Key, ProfileName, onfi_to_frame)
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
