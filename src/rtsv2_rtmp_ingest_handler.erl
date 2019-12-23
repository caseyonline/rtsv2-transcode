-module(rtsv2_rtmp_ingest_handler).

-include_lib("id3as_common/include/common.hrl").
-include_lib("id3as_common/include/id3as_types.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/frame.hrl").
-include_lib("id3as_media/include/rtmp.hrl").

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
         checkSlot :: fun(),
         slot :: term()
        }).

init(Rtmp, ConnectArgs, [#{ingestStarted := IngestStarted,
                           ingestStopped := IngestStopped,
                           checkSlot := CheckSlot}]) ->

  {_, TcUrl} = lists:keyfind("tcUrl", 1, ConnectArgs),

  #rtmp_parsed_url{
     connection_config = #rtmp_connection_config{
                            host = Host
                           },
     app_config = #rtmp_app_config{
                     %%application = Application
                    },
     %%stream_name = StreamName,
     query = Query
    } = rtmp_utils:parse_url(TcUrl),

  %% Stuff hardcoded until LLNW finalise the API...
  Application = <<"mmddev001">>,
  StreamName = <<"my-cool-slot_1000">>,
  Protocol = adobe,

  case Query of
    #{<<"authmod">> := <<"adobe">>,
      <<"user">> := User,
      <<"challenge">> := ClientChallenge,
      <<"response">> := ClientResponse} ->
      %% We have a adobe completed digest

      Reply = (((CheckSlot(Host))(Application))(StreamName))(),

      case Reply of
        {nothing} ->
          {stop, rejected};

        {just, Slot = #{slot := #{publishAuth := #{authType := {adobe},
                                                   username := ExpectedUserName,
                                                   password := ExpectedPassword}}}} ->

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
                           checkSlot = CheckSlot,
                           slot = Slot}};

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
      <<"user">> := User,
      <<"nonce">> := ClientOurNonce,
      <<"cnonce">> := ClientNonce,
      <<"nc">> := ClientNc,
      <<"response">> := ClientResponse} ->
      %% We have a llnw completed digest

      Reply = (((CheckSlot(Host))(Application))(StreamName))(),

      case Reply of
        {nothing} ->
          {stop, rejected};

        {just, Slot = #{slot := #{publishAuth := #{authType := {adobe},
                                                   username := ExpectedUserName,
                                                   password := ExpectedPassword}}}} ->

          Realm = <<"live">>,
          Method = <<"publish">>,
          Nonce = <<"ODE3MDQ3NTYz">>,
          Qop = <<"auth">>,

          case rtmp:compare_llnw_challenge_response(ExpectedUserName,
                                                    Realm,
                                                    ExpectedPassword,
                                                    Method,
                                                    Application,
                                                    Nonce,
                                                    ClientNc,
                                                    ClientNonce,
                                                    Qop,
                                                    ClientResponse) of
            true ->
              {ok, #?state{rtmp_pid = Rtmp,
                           ingestStarted = IngestStarted,
                           ingestStopped = IngestStopped,
                           checkSlot = CheckSlot,
                           slot = Slot}};

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
      ?INFO("No authmod - start authentication"),
      {authenticate, Protocol}
  end.

handle(State = #?state{rtmp_pid = Rtmp,
                       ingestStarted = IngestStarted,
                       slot = Slot}) ->

  receive
    {Rtmp, {request, publish, {StreamId, ClientId, Path}}} ->
      [StreamNameStr | _] = string:tokens(Path, "&"),

      ?SLOG_INFO("Inbound stream", #{stream_id => StreamId,
                                     client_id => ClientId,
                                     path => Path,
                                     stream_name => StreamNameStr}),

      StreamName = list_to_binary(StreamNameStr),

      unit = ((IngestStarted(Slot))(StreamName))(),

      {ok, WorkflowPid} = start_workflow(Rtmp),

      %% Stream is now connected - we block in here, when we return the rtmp_server instance will close
      workflow_loop(StreamName, WorkflowPid, State);

    {Rtmp, disconnected} ->
      ok
  end.

workflow_loop(StreamName, WorkflowPid, State = #?state{ingestStopped = IngestStopped,
                                                       slot = Slot}) ->
  %% the workflow is dealing with the RTMP, so just wait until it says we are done
  receive
    #workflow_output{message = #no_active_generators_msg{}} ->
      ?SLOG_WARNING("Client exited"),

      unit = ((IngestStopped(Slot))(StreamName))(),
      ok;

    Other ->
      ?SLOG_WARNING("Unexpected workflow output ~p", [Other]),
      workflow_loop(StreamName, WorkflowPid, State)
  end.

start_workflow(Rtmp) ->

  Workflow = #workflow{
                name = ingest,
                generators = [
                              #generator{name = rtmp_ingest,
                                         module = rtmp_push_ingest_generator,
                                         config = #rtmp_push_ingest_generator_config{
                                                     rtmp = Rtmp
                                                    }}
                             ],
                processors = [
                              #processor{name = demux,
                                         subscribes_to = rtmp_ingest,
                                         module = rtmp_tag_to_frame
                                        },

                              #processor{name = null,
                                         subscribes_to = ?previous,
                                         module = dev_null_processor}
                             ]
               },

  {ok, WorkflowPid} = id3as_workflow:start_link(Workflow),

  {ok, WorkflowPid}.
