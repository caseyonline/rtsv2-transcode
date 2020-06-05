%% Derived from ts_akamai_writer
%% POST -> PUT
%%%-------------------------------------------------------------------
%%% @author Steve Strong <steve@Steves-MacBook-Pro.local>
%%% @copyright (C) 2011, Steve Strong
%%% Created : 20 Dec 2011 by Steve Strong <steve@Steves-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(rtsv2_internal_hls_writer).

-behaviour(workflow_processor).

-include_lib("id3as_common/include/common.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("id3as_common/include/spud_gun.hrl").
-include_lib("id3as_common/include/id3as_types.hrl").
-include_lib("id3as_media/include/id3as_workflow.hrl").
-include_lib("id3as_media/include/ts.hrl").
-include_lib("id3as_media/include/ts_segment_generator.hrl").
-include_lib("id3as_media/include/m3u8.hrl").

-include("./rtsv2_internal_hls_writer.hrl").

%% Workflow API
-export([
         spec/1,
         initialise/1,
         process_input/2,
         handle_info/2,
         flush/1,
         ioctl/2,
         stop/1,
         is_meter/0
        ]).

-record(playlist_entry, {
          url :: binary_string(),
          duration_ms :: integer(),
          sequence_number :: integer(),
          source_timestamp :: undefined | milliseconds()
         }).
-type playlist_entry() :: #playlist_entry{}.

-record(state, {
          directory_prefix :: binary_string(),
          post_url :: undefined | binary_string(),
          auth :: {binary_string(), binary_string()},
          max_playlist_length :: integer(),
          segment_start_pts :: undefined | integer(),
          segment_start_timestamp :: undefined | milliseconds(),
          last_pts :: undefined | integer(),
          next_sequence_number :: undefined | integer(),
          max_entries_per_directory :: integer(),
          target_segment_duration :: integer(),
          current_segment :: undefined | iolist(),
          playlist_name :: binary_string(),
          playlist = queue:new() :: queue:queue(playlist_entry()),
          worker_pid :: pid(),
          stream_ended = false :: boolean()
         }).

-record(worker_state, {
          parent :: pid(),
          retry_count = 0 :: integer(),
          cached_host_ip ::  undefined | inet:ip_address(),
          cached_host :: undefined | binary_string(),
          spud_pid :: undefined | pid(),
          current_job :: undefined | posting_job(),
          version_string :: binary_string(),
          auth :: {binary_string(), binary_string()}
         }).

-record(posting_job, {
          url :: binary_string(),
          data :: iolist(),
          job_type :: manifest | segment,
          completion_message :: undefined | playlist_entry(),
          retry_timeout :: integer(),
          max_retry_attempts :: infinite | integer(),
          job_identifier :: binary_string()
         }).
-type posting_job() :: #posting_job{}.


%%------------------------------------------------------------------------------
%% Workflow API
%%------------------------------------------------------------------------------
spec(_Processor) ->
  #processor_spec{
     consumes = [?ts_stream_end_markers, ?ts_segment_start_markers, ?ts_segment_end_markers, ?ts_segment_datas]
    }.

initialise(Processor = #processor{config = Undefined}) when Undefined == [];
                                                            Undefined == undefined ->

  initialise(Processor#processor{config = #rtsv2_internal_hls_writer_config{}});

initialise(#processor{config = #rtsv2_internal_hls_writer_config{collision_protection_mode = ProtectionMode,
                                                                 post_url = PostUrl,
                                                                 auth = Auth,
                                                                 target_segment_duration = TargetSegmentDuration,
                                                                 max_entries_per_directory = MaxEntriesPerDirectory,
                                                                 playlist_name = PlaylistName,
                                                                 max_playlist_length = MaxPlaylistLength,
                                                                 version_string = VersionString}}) ->

  Prefix = case ProtectionMode of
             none -> <<>>;
             {directory_prefix, SpecifiedPrefix} -> SpecifiedPrefix
           end,

  State = #state{directory_prefix = Prefix,
                 post_url = PostUrl,
                 auth = Auth,
                 max_playlist_length = MaxPlaylistLength,
                 target_segment_duration = TargetSegmentDuration,
                 playlist_name = PlaylistName,
                 max_entries_per_directory = MaxEntriesPerDirectory,
                 worker_pid = start_worker_loop(VersionString, Auth)
                },

  {ok, State}.

process_input(_, State = #state{ post_url = PostUrl, stream_ended = true}) ->
  ?INFO("Akamai writer for ~p ignoring input as stream ended", [ PostUrl ]),
  {ok, State };

process_input(#ts_stream_end_marker{}, State = #state{post_url = PostUrl }) ->
  NewState = State#state { stream_ended = true },
  ?INFO("Sending final playlist for ~p", [ PostUrl ]),
  send_current_playlist(NewState),
  {ok, NewState };

process_input(#ts_segment_start_marker{start_pts = Pts,
                                       start_source_timestamp = Timestamp,
                                       sequence_number = SeqNumber}, State = #state{current_segment = undefined}) ->
                                          
  {ok, State#state{segment_start_pts = Pts,
                   segment_start_timestamp = Timestamp,
                   current_segment = [],
                   next_sequence_number = SeqNumber}};

process_input(Marker = #ts_segment_start_marker{sequence_number = SeqNumber}, State) ->

  %% We never really expect this case - a start_marker is always either the first thing
  %% that we see (so file == undefined), or comes after an end marker (so file == undefined).
  NewState = send_segment(Marker, State#state{next_sequence_number = SeqNumber}),

  process_input(Marker, NewState);

process_input(Marker = #ts_segment_end_marker{}, State) ->

  NewState = send_segment(Marker, State),

  {ok, NewState};

process_input(#ts_segment_data{data = Data,
                               pts = Pts}, State) ->

  State2 = add_to_current_segment(Data, State),

  {ok, State2#state{last_pts = Pts}}.

handle_info(NewPlaylistEntry = #playlist_entry{}, State) ->
  NewState = add_entry_to_playlist(NewPlaylistEntry, State),
  send_current_playlist(NewState),
  {noreply, NewState}.

flush(State = #state { worker_pid = WorkerPid }) ->
  WorkerPid ! flush,
  receive
    flush_complete ->
      {flush_complete, State}
  after 1000 ->
    {flush_complete, State}
  end.

-spec ioctl(read_meter, term()) -> {ok, status(), term()}.
ioctl(read_meter, State = #state{post_url = PostUrl,
                                 directory_prefix = Prefix,
                                 max_entries_per_directory = MaxEntriesPerDirectory,
                                 next_sequence_number = NextSequenceNumber
                                }) ->

  NextDirectoryNumber = NextSequenceNumber div MaxEntriesPerDirectory,

  NextChunk = <<Prefix/binary, (integer_to_binary(NextDirectoryNumber))/binary, "/file", (integer_to_binary(NextSequenceNumber))/binary, ".ts">>,

  Metrics = [
             #text_metric{name = post_url, value = PostUrl, display_name = <<"Post Url">>, interpret_as = url, update_frequency = low},
             #text_metric{name = next_chunk, value = NextChunk, display_name = <<"Next Chunk">>, update_frequency = high}
            ],

  {ok, #status{metrics = Metrics}, State}.

is_meter() ->
  true.

stop(State) ->
  _ = close(State),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
close(State) ->
  send_segment(State).


add_to_current_segment(undefined, State) ->
  State;

add_to_current_segment(<<>>, State) ->
  State;

add_to_current_segment(_Data, State = #state{current_segment = undefined}) ->
  State;

add_to_current_segment(Data, State = #state{current_segment = CurrentBuffer}) ->
  State#state{current_segment = [CurrentBuffer, Data]}.


send_segment(State = #state{last_pts = LastPts}) ->
  send_segment(LastPts, State).

send_segment(#ts_segment_end_marker{end_pts = EndPts}, State) ->
  send_segment(EndPts, State);

send_segment(_EndPts, State = #state{current_segment = undefined}) ->
  State;

send_segment(EndPts, State = #state{post_url = PostUrl,
                                    current_segment = CurrentSegment,
                                    worker_pid = WorkerPid}) ->

  PlaylistEntry = build_playlist_entry(EndPts, State),
  SegmentUrl = <<PostUrl/binary, (PlaylistEntry#playlist_entry.url)/binary>>,
  SegmentJob = #posting_job {
                          url = SegmentUrl,
                          data = CurrentSegment,
                          job_type = segment,
                          job_identifier = PostUrl,
                          completion_message = PlaylistEntry,
                          retry_timeout = 500,
                          max_retry_attempts = 3
                         },
  WorkerPid !  SegmentJob,
  setup_for_next_segment(State).

setup_for_next_segment(State) ->
  State#state{current_segment = undefined,
              segment_start_pts = undefined,
              segment_start_timestamp = undefined,
              last_pts = undefined
             }.

send_current_playlist(#state { playlist = Playlist,
                               post_url = PostUrl,
                               worker_pid = WorkerPid,
                               target_segment_duration = TargetSegmentDuration,
                               playlist_name = PlaylistName,
                               stream_ended = StreamEnded}) ->

  case queue:is_empty(Playlist) of
    true -> ok;
    false ->
      #playlist_entry{sequence_number = FirstSeqNo} = queue:get(Playlist),

      Entries = [
                 #media_playlist_segment_entry {
                    duration_ms = Duration,
                    program_date_time = SourceTimestamp,
                    uri = Url
                   } ||
                 #playlist_entry{duration_ms = Duration,
                                 source_timestamp = SourceTimestamp,
                                 url = Url} <- queue:to_list(Playlist)
                ],

      M3u8Playlist = #media_playlist {
                        is_final_playlist = StreamEnded,
                        header = #media_playlist_header {
                                    version = 3,
                                    max_duration_ms = TargetSegmentDuration * 1000,
                                    allow_cache = undefined,
                                    initial_sequence_number = FirstSeqNo
                                   },
                        entries = Entries
                       },

      M3u8 = m3u8:media_playlist(M3u8Playlist),
      PlaylistUrl = <<PostUrl/binary, PlaylistName/binary >>,

      WorkerPid ! #posting_job {
                     url = PlaylistUrl,
                     data = M3u8,
                     job_type = manifest,
                     job_identifier = PostUrl,
                     retry_timeout = 1000,
                     max_retry_attempts = infinite
                    }
  end.

add_entry_to_playlist(PlaylistEntry, State = #state{playlist = Playlist,
                                                    max_playlist_length = MaxPlaylistLength}) ->

  Playlist2 = case queue:len(Playlist) of
                Ok when Ok < MaxPlaylistLength ->
                  Playlist;
                _ ->
                  queue:drop(Playlist)
              end,

  Playlist3 = queue:in(PlaylistEntry, Playlist2),

  State#state{playlist = Playlist3}.

build_playlist_entry(EndPts, #state{directory_prefix = Prefix,
                                    segment_start_pts = StartPts,
                                    segment_start_timestamp = Timestamp,
                                    next_sequence_number = SequenceNumber,
                                    max_entries_per_directory = MaxEntriesPerDirectory
                                   }) ->

  DirectoryNumber = SequenceNumber div MaxEntriesPerDirectory,

  ThisUrl = <<Prefix/binary, (integer_to_binary(DirectoryNumber))/binary, "/file_", (integer_to_binary(SequenceNumber))/binary, ".ts">>,

  PlaylistEntry = #playlist_entry{
                     url = ThisUrl,
                     duration_ms = trunc((EndPts - StartPts) / 90),
                     source_timestamp = Timestamp,
                     sequence_number = SequenceNumber
                    },
  PlaylistEntry.


start_worker_loop(VersionString, Auth) ->
  Self = self(),
  spawn_link(fun() ->
                 worker_loop(#worker_state { parent = Self, version_string = VersionString, auth = Auth })
             end).

worker_loop(State = #worker_state { retry_count = RetryCount, current_job = #posting_job { job_identifier = JobId,
                                                                                           job_type = JobType,
                                                                                           max_retry_attempts = MaxRetries }}) when RetryCount >= MaxRetries ->
  ?WARNING("Giving up on ~p in ~p, as max re-tries was reached", [ JobType, JobId ]),
  worker_loop(State#worker_state { spud_pid = undefined, current_job = undefined });

worker_loop(State = #worker_state {
                       parent = ParentPid,
                       current_job = CurrentJob
                      }) ->
  LoopTimeout = case CurrentJob of
          undefined -> 10000;
          #posting_job { retry_timeout = Timeout } -> Timeout
        end,

  NewState = receive
               #posting_job{ job_type = manifest } when CurrentJob =/= undefined ->
                 ?WARNING("Ignoring playlist post as it arrived after the next segment started"),
                 State;
               Job = #posting_job{} ->
                 do_posting_work(State#worker_state {
                                   current_job = Job,
                                   retry_count = 0
                                  });

               flush when CurrentJob =/= undefined ->
                 TriedAgain = do_posting_work(State), %% give it another shot
                 ParentPid ! flush_complete,
                 TriedAgain;

               flush ->
                 ParentPid ! flush_complete,
                 State
             after LoopTimeout ->
                do_posting_work(State)
             end,
  worker_loop(NewState).

do_posting_work(State = #worker_state { current_job = undefined }) -> State;
do_posting_work(State = #worker_state {
                           cached_host_ip = undefined,
                           retry_count = Retries
                          }) ->
  case resolve_host(State) of
    #worker_state { cached_host_ip = undefined,
                    current_job = #posting_job { url = Url }} ->
      ?WARNING("Failed to resolve ~p, starting retries", [Url]),
      State#worker_state { retry_count = Retries + 1 };
    NewState -> do_posting_work(NewState)
  end;

do_posting_work(State = #worker_state {
                           parent = Parent,
                           spud_pid = SpudPid,
                           cached_host = CachedHost,
                           cached_host_ip = CachedHostIp,
                           auth = {Username, Password},
                           version_string = VersionString,
                           current_job = #posting_job {
                                            job_identifier = JobIdentifier,
                                            url = Url,
                                            data = Data,
                                            job_type = JobType,
                                            completion_message = CompletionMessage,
                                            max_retry_attempts = MaxRetries
                                           },
                           retry_count = Retries
                          }) ->
  if
    Retries >= 1 -> ?INFO("Retrying ~p post to ~s (~p/~p)", [ JobType, Url, Retries+1, MaxRetries ]);
    ?otherwise -> ok
  end,
  BasicAuth = base64:encode(<<Username/binary,":",Password/binary>>),

  case
    manual_post(SpudPid, Url,
                CachedHostIp,
                [ { <<"host">>, CachedHost },
                  { <<"connection">>, <<"keep-alive">> },
                  { <<"content-type">>, <<"application/octet-stream">>},
                  { <<"authorization">>, <<"Basic ", BasicAuth/binary>>}

                  % { <<"user-agent">>, ?AKAMAI_USER_AGENT(VersionString) }
                ],
                Data
               )
  of
    {ok, SuccessStatus, _, NewSpudPid} when SuccessStatus >= 200 andalso SuccessStatus =< 299 ->
      if 
        Retries >= 1 -> ?INFO("Successful retry of ~p post to ~s (~p/~p)", [ JobType, Url, Retries+1, MaxRetries ]);
        ?otherwise -> ok
      end,
      if CompletionMessage =/= undefined -> Parent ! CompletionMessage;
         ?otherwise -> ok
      end,
      State#worker_state { spud_pid = NewSpudPid, current_job = undefined };

    {ok, ServerErrorStatus, Body, _} when ServerErrorStatus == 400 orelse ServerErrorStatus == 403 ->
      ?RAISE_SILENT_FLAG("~p post to ~s returned ~p (Check logs for details), not re-trying", [JobType, JobIdentifier, ServerErrorStatus]),
      ?WARNING("~p post to ~s returned (~p) ~p", [ JobType, Url, ServerErrorStatus, Body ]),
      begin_retry(State);

    {ok, ServerErrorStatus, _,  _} when ServerErrorStatus >= 500 andalso ServerErrorStatus =< 599 ->
      ?RAISE_FLAG("~p post to ~s returned ~p", [JobType, JobIdentifier, ServerErrorStatus]),
      ?WARNING("~p post to ~s returned ~p", [JobType, Url, ServerErrorStatus]),
      begin_retry(State);

    {ok, OtherStatus, _, _} ->
      ?RAISE_FLAG("~p post to ~s returned ~p, ignoring segment", [JobType, JobIdentifier, OtherStatus ]),
      ?WARNING("~p post to ~s returned ~p, ignoring segment", [JobType, Url, OtherStatus ]),
      State#worker_state { current_job = undefined };

    {error, Timeout} when Timeout == connect_timeout orelse Timeout == body_timeout orelse Timeout == request_timeout ->
      ?RAISE_FLAG("~p post to ~s timed out with ~p", [JobType, JobIdentifier, Timeout]),
      ?WARNING("~p post to ~s timed out with ~p", [JobType, Url, Timeout]),
      begin_retry(State#worker_state { spud_pid = undefined });

    { error, noproc } ->
      ?NOTICE("Lost persistent connection for ~s (~p), re-opening", [ Url, no_proc ]),
      begin_retry(State#worker_state { spud_pid = undefined });

    { error, connection_closed } ->
      ?NOTICE("Lost persistent connection for ~s (~p), re-opening", [ Url, connection_closed ]),
      begin_retry(State#worker_state { spud_pid = undefined });

    Other ->
      ?RAISE_SILENT_FLAG(" post to ~s returned an unknown result, check logs for details", [JobIdentifier]),
      ?WARNING("~p post to ~s returned ~p", [ JobType, Url, Other ]),
      State#worker_state { spud_pid = undefined, current_job = undefined }
  end.

begin_retry(State = #worker_state { retry_count = Retries, spud_pid = undefined }) ->
  State#worker_state {
    cached_host_ip = undefined,
    retry_count = Retries + 1
   };
begin_retry(State = #worker_state { spud_pid = SpudPid }) ->
  spud_gun:close(SpudPid), %% just make sure we don't leak
  begin_retry(State#worker_state { spud_pid = undefined }).

manual_post(SpudPid, Url, HostIp, Headers, Data) ->
  SimpleRequest = spud_gun:url_to_request_record(Url, #spud_request {
                                                         method = put,
                                                         body = Data,
                                                         headers = Headers,
                                                         spud_pid = SpudPid
                                                        }),
  SimpleRequestWithCachedIp = SimpleRequest#spud_request { remote_host = i_convert:convert(HostIp, binary_string), return_spud_response = true },
  case spud_gun:simple_request(SimpleRequestWithCachedIp) of
    { ok, #spud_response {
             http_code = Status,
             spud_pid = NewSpudPid,
             headers = _OutputHeaders,
             body = Body
            }} ->
      {ok, Status, Body, NewSpudPid };
    { error, Reason } ->
      { error, Reason }
  end.

resolve_host(State = #worker_state { current_job = #posting_job { url = Url }}) ->
  { ok, {_Scheme, _UserInfo, Host, _Port, _Path, _Query} } = http_uri:parse(binary_to_list(Url)),
  case inet:gethostbyname(Host) of
    { ok, #hostent { h_addr_list = [ Head | _ ] } } ->
      ?INFO("Resolved ~p to ~p (for ~p), and cacheing result", [ Host, Head, Url ]),
      State#worker_state { cached_host_ip = Head, cached_host = list_to_binary(Host) };
    _ ->
      State
  end.
