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
         workflow_pid :: pid(),
         ingestStarted :: fun(),
         ingestStopped :: fun(),
         checkSlot :: fun()
        }).

%% TODO:
%% - auth
%%   - rtmp.erl calls into rtmp_server.erl which in turn calls this handler - so init here can call into purs
%%     with the connectArgs, and then call the LLNW api to get back the auth details (type, username, password)
%%     We then need a new return type of {authenticate, type, username, password} that rtmp_server passes
%%     back to rtmp.  After auth completes, rtmp_server is going to need to call back into this module to
%%     indicate success / failure - need to understand the flow for that
%% - IngestStarted / IngestStopped can pass StreamDetails around
%% - on publish, we need a callback to enable us to kill it if we don't like the stream name
%%   - will need to be a hook in #rtmp_push_ingest_generator_config{}
%% - need to know which variant we are...
%% - status reporting

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

  %% Init is called once the stream is connected.  Here we can call back into purs
  %% to get slot details, auth types etc, and fail the stream if we wish...

  Application = <<"mmddev001">>,
  StreamName = <<"my-cool-slot_1000">>,
  Protocol = llnw,

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

        {just, #{slot := #{publishAuth := #{authType := {adobe},
                                            username := ExpectedUserName,
                                            password := ExpectedPassword}}}} ->

          case rtmp:compare_abobe_challenge_response(ExpectedUserName,
                                                     <<"NzEwNzk">>,
                                                     ExpectedPassword,
                                                     <<"ODE3MDQ3NTYz">>,
                                                     ClientChallenge,
                                                     ClientResponse) of
            true ->
              {ok, State} = start_workflow(Rtmp),
              {ok, State#?state{ingestStarted = IngestStarted,
                                ingestStopped = IngestStopped,
                                checkSlot = CheckSlot}};

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

        {just, #{slot := #{publishAuth := #{authType := {adobe},
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
              {ok, State} = start_workflow(Rtmp),
              {ok, State#?state{ingestStarted = IngestStarted,
                                ingestStopped = IngestStopped,
                                checkSlot = CheckSlot}};

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

  %% Reply = (((CheckSlot(Host))(Application))(StreamName))(),

  %% case Reply of
  %%   {nothing} ->
  %%     {stop, rejected};

  %%   {just, #{slot := #{publishAuth := #{authType := {adobe},
  %%                                       username := ExpectedUserName,
  %%                                       password := ExpectedPassword}}}} ->

  %%     case maps:find(<<"authmod">>, Query) of
  %%       error ->
  %%         ?INFO("No authmod - start authentication"),
  %%         {authenticate, Protocol};

  %%       {ok, <<"adobe">>} ->
  %%         case maps:find(<<"challenge">>, Query) of
  %%           error ->
  %%             case maps:find(<<"user">>, Query) of
  %%               error ->
  %%                 ?INFO("No username - start authentication"),
  %%                 {authenticate, adobe};

  %%               {ok, UserName} when UserName == ExpectedUserName ->
  %%                 ?INFO("Username Matches - do next phase"),
  %%                 {authenticate, adobe, UserName, <<"NzEwNzk">>, <<"ODE3MDQ3NTYz">>};

  %%               _ ->
  %%                 ?INFO("Username does not match, fail"),
  %%                 {authenticate_fail, adobe, unknown_user}
  %%             end;

  %%           {ok, ClientChallenge} ->
  %%             case maps:find(<<"response">>, Query) of
  %%               error ->
  %%                 ?INFO("Challenge but not response, fail"),
  %%                 {stop, rejected};
  %%               {ok, ClientResponse} ->
  %%                 case rtmp:compare_challenge_response(ExpectedUserName,
  %%                                                      <<"NzEwNzk">>,
  %%                                                      ExpectedPassword,
  %%                                                      <<"ODE3MDQ3NTYz">>,
  %%                                                      ClientChallenge,
  %%                                                      ClientResponse) of
  %%                   true ->
  %%                     {ok, State} = start_workflow(Rtmp),
  %%                     {ok, State#?state{ingestStarted = IngestStarted,
  %%                                       ingestStopped = IngestStopped,
  %%                                       checkSlot = CheckSlot}};

  %%                   false ->
  %%                     ?SLOG_INFO("Challenge failed", #{client_challenge => ClientChallenge,
  %%                                                      client_response => ClientResponse}),
  %%                     {authenticate_fail, adobe, invalid_password}
  %%                 end
  %%             end
  %%         end;

  %%       _ ->
  %%         ?INFO("Authmod does not match, fail"),
  %%         {stop, rejected}
  %%     end;

  %%   {just, StreamDetails} ->
  %%     {ok, State} = start_workflow(Rtmp),

  %%     {ok, State#?state{ingestStarted = IngestStarted,
  %%                       ingestStopped = IngestStopped,
  %%                       checkSlot = CheckSlot}}
  %% end.

handle(State = #?state{ingestStarted = IngestStarted,
                       ingestStopped = IngestStopped}) ->

  %% Stream is now connected - we block in here, when we return the rtmp_server instance will close

  %% TODO - obvs stream name etc
  unit = ((IngestStarted(<<"stream1">>))(<<"high">>))(),

  %% the workflow is dealing with the RTMP, so just wait until it says we are done
  receive
    #workflow_output{message = #no_active_generators_msg{}} ->
      ?SLOG_WARNING("Client exited"),
      %% TODO - obvs stream name etc
      unit = ((IngestStopped(<<"stream1">>))(<<"high">>))(),
      ok;

    Other ->
      ?SLOG_WARNING("Unexpected workflow output ~p", [Other]),
      handle(State)
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

  {ok, #?state{workflow_pid = WorkflowPid}}.
